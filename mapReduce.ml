
(* Naive implemntation :
module Base (K : Kahn.S) =
struct
  let (>>=) = K.bind

  let new_worker worker v oc =
    K.return v
    >>= (fun v -> K.put (worker v) oc)

  let compute worker master init =
    let rec process values =
      let l =
	List.map (fun (k, v) ->
	  let (recv_inter, send_inter) = K.new_channel () in
	  let w = new_worker worker v send_inter in
	  (w, ((k, v), recv_inter))
	) values
      in
      let (workers, inter) = List.split l in
      K.doco (receive_values inter :: workers)
    and receive_values l =
      match l with
      | [] -> K.return ()
      | (k, recv) :: others ->
	 K.get recv
	 >>= (fun v -> process (master k v))
	 >>= (fun () -> receive_values others)
    in K.run (process init)
end
*)

module type BASE_TYPE =
sig
  val compute : num_workers:int -> ('a -> 'b) -> (('c * 'a) -> 'b -> ('c * 'a) list) -> ('c * 'a) list -> unit
end  

module Base (K : Kahn.S) : BASE_TYPE =
struct
  let (>>=) = K.bind

  type 'a job =
    | Work of 'a
    | Stop

  let do_job worker oc cont j =
    match j with
    | Work x -> (K.put (worker x) oc >>= cont)
    | Stop -> K.return ()

  let new_worker worker ic oc =
    let rec impl () =
      K.get ic
      >>= (do_job worker oc impl)
    in impl ()

  let compute ~num_workers worker master init =
    let channels = Array.init num_workers (fun _ ->
      let (recv_init, send_init) = K.new_channel () in
      let (recv_inter, send_inter) = K.new_channel () in
      let w = new_worker worker recv_init send_inter in
      (w, (send_init, recv_inter))
    ) in

    let send_chan (_, (sc, _)) = sc in
    let recv_chan (_, (_, rc)) = rc in

    let ongoing = Array.init num_workers (fun _ -> None) in

    (* Envoie les taches aux workers *)
    let rec process wcounter values =
      match values with
      | [] -> receive_values 0 []
      | (k, v) :: others ->
	 let rec impl i =
	   if i < num_workers
	   then
	     begin
	       match ongoing.(i) with
	       | Some _ -> impl (succ i)
	       | None ->
		  begin
		    ongoing.(i) <- Some (k, v);
		    let sc = send_chan channels.(i) in
		    K.put (Work v) sc
		    >>= (fun () -> process (succ wcounter mod 8) others)
		  end
	     end
	   else receive_values 0 values
	 in impl wcounter
    (* Recoit les résultat des calculs des worker ; effectue master *)
    and receive_values i l =
      if i < num_workers
      then
	let keys = ongoing.(i) in
	match keys with
	| None -> receive_values (succ i) l
	| Some k ->
	  let rc = recv_chan channels.(i) in
	  K.get rc
	  >>= (fun v ->
	    ongoing.(i) <- None ;
	    receive_values (succ i) ((master k v) @ l))
      else
	if l = []
	then terminate ()
	else process 0 l
    (* Stoppe tous les workers, puis quitte *)
    and terminate () =
      Array.fold_left
	(fun process (_, (sc, _)) ->
	  K.put Stop sc
	  >>= (fun () -> process)
	) (K.return ()) channels
    in
    let workers = Array.map fst channels |> Array.to_list in

    (* Le seul processus garanti de rester vivant est le dernier *) 
    K.run (K.doco (workers @ [process 0 init]))

end



module MapReduce (B : BASE_TYPE) =
struct
  type ('a, 'b) t =
    | Map of 'a
    | Red of 'b

  let map_reduce ~num_workers map reduce init default =

    let reduced = Hashtbl.create (List.length init) in

    let reduced_value k =
      try Some (Hashtbl.find reduced k)
      with Not_found -> None
    in

    let worker x =
      match x with
      | Map x -> Map (map x)
      | Red (acc, x) -> Red (reduce acc x)
    in

    let master (k, v) comp =
      match (k,  comp) with
      | (Map _, Map inter) ->
	 List.fold_left (fun tasks (inter_k, inter_v) ->
	   let red = reduced_value inter_k in
	   match red with
	   | None -> ((Hashtbl.add reduced inter_k inter_v) ; tasks)
	   | Some acc -> (Hashtbl.remove reduced inter_k ;
			  (Red inter_k, Red (acc, inter_v)) :: tasks)
	 ) [] inter
      | (Red k, Red red) ->
	 let curr = reduced_value k in
	 begin
	   match curr with
	   | None -> (Hashtbl.add reduced k red ; [])
	   | Some acc -> (Hashtbl.remove reduced k ; [Red k, Red (acc, red)])
	 end
      | _ -> assert false
    in

    let init = List.map (fun (k, v) -> (Map k, Map v)) init in

    B.compute ~num_workers worker master init ;
    reduced
end


let count_word_length kahn num_workers words =
  let module K = (val kahn : Kahn.S) in
  let module B = Base (K) in
  let module M = MapReduce (B) in
  M.map_reduce ~num_workers
    (fun w -> [String.length w, 1]) (+)
    (List.map (fun w -> ((), w)) words) 0

let words f =
  let scanbuf = Scanf.Scanning.open_in f in
  let rec read acc =
    let w = Scanf.bscanf scanbuf "%s@\n" (fun s -> s) in
    if w = ""
    then acc
    else read (w :: acc)
  in read []

let kahn_networks =
  ["sequentiel"    , (module Sequentiel.I : Kahn.S) ;
   "pipes"         , (module Pipes.I : Kahn.S) ;
   "network"       , (module Network.I : Kahn.S) ;
   "thread"        , (module Kahn.Th : Kahn.S) ;
   "pipes-thread"  , (module PipesThread.I : Kahn.S) ;
   "network-thread", (module NetworkThread.I : Kahn.S)]

let kahn_names = List.split kahn_networks |> fst

let set_kahn k n =
  k := List.assoc n kahn_networks
    
let _ =
  let kahn = ref (module Pipes.I : Kahn.S) in
  let num_workers = ref 50 in
  let silent = ref false in
  let file = ref "/usr/share/dict/words" in
  Arg.(parse
	 ["-workers", Set_int num_workers, "Numvber of workers" ;
	  "-silent", Set silent, "Don't display computation results" ;
	  "-network", Symbol (kahn_names, set_kahn kahn), "Implementation to use" ;
	  "-file", Set_string file, "File to count the words from"
	 ]
	 
	 (fun _ -> ()) "") ;
  
  let text = words !file in
  let distrib = count_word_length !kahn !num_workers text in
  if not !silent
  then
    let distrib =
      Hashtbl.fold (fun wl count acc -> (count, wl) :: acc) distrib []
	 |> List.sort compare
    in
    List.iter (fun (count, wl) ->
      Printf.printf "%7d words have length %2d\n" count wl
    ) distrib


      
(* Multiplication matricielle à l'aide de l'interface en style functory *)
module B = Base (Sequentiel.I)


module MatrixMultiplication =
struct
  let multiply a b =
    let m = Array.length a in
    let n = Array.length b in
    let p = Array.length a.(0) in

    let c = Array.make_matrix m n 0 in

    let tasks =
      let l = ref [] in
      for i = 0 to m-1
      do
	for j = 0 to n-1
	do
	  l := ((i, j), (a.(i), b.(j))) :: !l
	done
      done ;
      !l
    in

    let worker (ai, bj) =
      let c = ref 0 in
      for k = 0 to p-1
      do
	c := !c + ai.(k) * bj.(k)
      done ;
      !c
    in

    let master ((i, j), _) r = c.(i).(j) <- r ; [] in

    B.compute ~num_workers:8 worker master tasks ;
    c

end

(*
let print_matrix a =
  Array.iter (fun c ->
    Array.iter (Printf.printf "%d ") c ;
    Printf.printf "\n"
  ) a

let print_transpose a =
  let m = Array.length a in
  let n = Array.length a.(0) in
  for i = 0 to n-1
  do
    for j = 0 to m-1
    do
      Printf.printf "%d " a.(j).(i)
    done ;
    Printf.printf "\n"
  done

let _ =
  let m = 15 in
  let n = 14 in
  let p = 30 in

  let matrix m p =
    Array.init m (fun _ -> Array.init p (fun _ -> Random.int 5))
  in
  let a = matrix m p in
  let b = matrix n p in
  let c = MatrixMultiplication.multiply a b in
  Unix.sleep 1 ;
  print_matrix a ;
  Printf.printf "\n X \n\n" ;
  print_transpose b ;
  Printf.printf "\n = \n\n" ;
  print_matrix c
  *)
