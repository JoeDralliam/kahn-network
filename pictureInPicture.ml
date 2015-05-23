module Image =
struct
  type color = int * int * int
  type image =
    { width: int;
      height: int;
      pixels: color list; } 

(* ========================================================== *)
(* Lire une image au format ppm                               *)
(* ========================================================== *)
  let input_int chan =
    let rec input_int c acc =
      if '0' <= c && c <= '9' then 
	input_int 
	  (input_char chan) 
	  (10 * acc + (int_of_char c - int_of_char '0'))
      else
	acc
    in
    let c = input_char chan in
    if not ('0' <= c && c <= '9') then failwith "input_int"
    else
      let n = input_int c 0 in
      let _ = seek_in chan (pos_in chan - 1) in
      n      
	
  let rec whitespace chan =
    match input_char chan with
    | ' ' | '\t' | '\r' | '\n' -> whitespace chan
    | _ -> seek_in chan (pos_in chan - 1)
       
       
  let input_color chan = 
    let r = int_of_char (input_char chan) in
    let g = int_of_char (input_char chan) in
    let b = int_of_char (input_char chan) in
    (r,g,b)
      
      
  (* ========================================================== *)
  (* Afficher une image à l'écran                               *)
  (* ========================================================== *)

  let draw_line width j pixels =
    let rec draw_line i pixels =
      if i >= width then pixels
      else
	match pixels with
	| [] -> prerr_endline "draw_line: bad image size !"; []
	| (r,g,b) :: l -> 
	   let _ = Graphics.set_color (Graphics.rgb r g b) in
	   let _ = Graphics.plot i j in
	  draw_line (i + 1) l
    in
    draw_line 0 pixels
      
      
  let draw_image img =
    let rec draw_pixels j pixels =
      if j < 0 then 
	if pixels <> [] then prerr_endline "draw_image: bad image size !"
	else ()
      else 
	let pixels' = draw_line img.width j pixels in
	draw_pixels (j - 1) pixels'
    in
    let _ = Graphics.clear_graph () in 
    let _ = Graphics.resize_window img.width img.height in 
    let _ = draw_pixels (img.height - 1) img.pixels in
    Graphics.synchronize ()

end

module Make (K : Kahn.S) =
struct
  type image =
    { width: int;
      height: int;
      pixels: Image.color K.in_port; } 


  let (>>=) = K.bind
  let return = K.return

  let put2 v (out1, out2) =
    K.put v out1
    >>= (fun () -> K.put v out2)

  let delay f x = (fun () -> return (f x))
    
  let rec input_color_line chan width out () =
    if width > 0
    then
      K.put (Image.input_color chan) out
       >>= input_color_line chan (width - 1) out
    else
      return ()
      
  let input_color_matrix chan height width =
    let (res, out) = K.new_channel () in
    let rec input_color_matrix n () =
      if n > 0
      then
	input_color_line chan width out ()
	 >>= input_color_matrix (n - 1)
      else return ()
    in
    (input_color_matrix height (), res)
      
  let rec input_ppm chan =
    if input_char chan <> 'P' then failwith "input: bad format" ;
    if input_char chan <> '6' then failwith "input: bad format" ;
    Image.whitespace chan ;
    let width = Image.input_int chan in
    Image.whitespace chan ;
    let height = Image.input_int chan in 
    Image.whitespace chan ;
    let maxval = Image.input_int chan in
    if maxval >= 256 then failwith "input: too much colors";
    Image.whitespace chan;
    let (pinput, pixels) = input_color_matrix chan height width in
    (pinput,
     { width = width; 
       height = height; 
       pixels = pixels; })
      
  let read_ppm file =
    let chan = open_in_bin file in
    let (pinput, img) = input_ppm chan in
    (pinput >>= delay close_in chan, img)

      

  let vector_of_matrix size m =
    let (res, v) = K.new_channel () in
    let rec impl i () =
      if i < snd size
      then
	K.get m
	>>= (fun line ->
	 List.map (fun x -> (fun () -> K.put x v)) line
         |> List.fold_left K.bind (return ())
	 >>= impl (succ i)
	)
      else return ()
    in
    (impl 0 (), res)

  let get_line size v =
    let rec get i acc =
      if i < size
      then
	K.get v
	>>= (fun x -> return (x :: acc))
	>>= get (succ i)
      else return (List.rev acc)
    in
    get 0 []
      
  let matrix_of_vector v (sizex, sizey) =
    let (res, m) = K.new_channel () in
    let rec impl j () =
      if j < sizey
      then 
	get_line sizex v
	 >>= (fun l ->
	   K.put l m
	   >>= impl (succ j))
      else return ()
    in
    (impl 0 (), res)
    

      
  let build_mask (bsizex, bsizey) (ssizex, ssizey) =
    let (in1, out1) = K.new_channel () in
    let (in2, out2) = K.new_channel () in
    let out = (out1, out2) in
    if bsizex < ssizex || bsizey < ssizey then
      failwith "mask: incompatible sizes"
    else
      let rec full_line i () =
	if i < bsizex
	then
	  put2 true out
	  >>= full_line (i+1)
	else return ()
      in
      let rec not_full_line i () =
	if i < ssizex
	then
	  put2 false out
	  >>= not_full_line (i+1)
	else if i < bsizex
	then
	  put2 true out
	  >>= not_full_line (i+1)
	else return ()
      in
      let rec build j () =
	if j < 0
	then return ()
	else if j < ssizey
	then
	  not_full_line 0 ()
	  >>= build (pred j)
	else
	  full_line 0 ()
	  >>= build (pred j)
      in
      (build (bsizey - 1) (), in1, in2)
      
  let sampler size mask l =
    let (res, out) = K.new_channel () in
    let rec impl i () =
      if i < size
      then
	K.get l
	>>= (fun x -> K.get mask
	>>= (fun keep ->
	  (if keep then K.put x out else return ())
          >>= impl (succ i)
	))
      else return ()
    in (impl 0 (), res)

  let merge size mask l1 l2 =
    let (res, out) = K.new_channel () in
    let rec impl i () =
      if i < size
      then
	K.get mask
	>>= (fun m ->
	  (if m then K.get l1 else K.get l2)
	  >>= (fun x -> K.put x out)
	  >>= impl (succ i)
	)
      else return ()
    in (impl 0 (), res)


  let half_sizer size m =
    let (res, out) = K.new_channel () in
    
    let rec keep l acc =
      match l with
      | [] -> List.rev acc
      | x :: l -> throw l (x :: acc)
    and throw l acc =
      match l with
      | [] -> acc
      | _ :: l -> keep l acc
    in
    let h = snd size in
    
    let rec keep' i () =
      if i < h
      then
	K.get m
	 >>= (fun l -> K.put (keep l []) out)
	 >>= throw' (succ i)
      else return ()
    and throw' i () =
      if i < h
      then
	K.get m
	 >>= (fun _ -> keep' (succ i) ())
      else return ()
    in
    (keep' 0 (), res)
      
    

  let recv_list size l =
    let rec impl i acc =
      if i < size
      then
	K.get l
         >>= (fun x -> impl (succ i) (x :: acc))
      else return (List.rev acc)
    in impl 0 []
  
  let display silent size l =
    recv_list (fst size * snd size) l
    >>= (fun pixels ->
      Image.(
	if not silent
	then draw_image { width = fst size ; height = snd size ; pixels }) ;
      return ()
    )



      
  let send_list lst =
    let (res, out) = K.new_channel () in
    (List.map (fun p () -> K.put p out) lst
     |> List.fold_left K.bind (return ()), res)
      
      
  let size img = (img.width, img.height)
  let half_size img = ((img.width + 1) / 2, (img.height + 1) / 2)
  let lin_size img = img.width * img.height
    
  let pip_impl big small =
    let (pmat, lsmall) = matrix_of_vector small.pixels (size small) in
    let (phalf, lsmall) = half_sizer (size small) lsmall in
    let (pvec, lsmall) = vector_of_matrix (half_size small) lsmall in 
    let (pmask, mask1, mask2) = build_mask (size big) (half_size small) in
    let (psamp, lbig) = sampler (lin_size big) mask1 big.pixels in
    let (pmerge, lmerge) = merge (lin_size big) mask2 lbig lsmall in
    (K.doco [pmat ; phalf ; pvec ; pmask ; psamp ; pmerge], lmerge)

  let pip silent big small =
    let (pbig, big) = read_ppm big in
    let (psmall, small) = read_ppm small in
    let (ppip, res) = pip_impl big small in
    let pdisplay = display silent (size big) res in
    K.run (K.doco [pbig ; psmall ; ppip ; pdisplay])
      
  let pip_n silent n img =
    let rec impl k =
      if k = 1
      then read_ppm img
      else
	let (pbig, big) = read_ppm img in
	let (psmall, small) = impl (pred k) in
	let (pfus, res) = pip_impl big small in
	(K.doco [pbig ; psmall ; pfus], { big with pixels = res})
    in
    let (pimpl, res) = impl n in
    let pdisplay = display silent (size res) res.pixels in 
    K.run (K.doco [pimpl ; pdisplay])
end

module Pip = Make(Pipes.I)

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
  let big = ref "bigimg.ppm" in
  let small = ref "" in
  let silent = ref false in
  let recursivity = ref 10 in
  Arg.(parse
	 [
	  "-silent", Set silent, "Don't display computation results" ;
	   "-network", Symbol (kahn_names, set_kahn kahn), "Implementation to use" ;
	   "-big", Set_string big, "Big image" ;
	   "-small", Set_string small, "Small image" ;
	   "-recursivity", Set_int recursivity, "Number of recursive image put together";
	 ]
	 
	 (fun _ -> ()) "") ;
  


  if not !silent
  then
    begin
      Graphics.open_graph "";
      Graphics.auto_synchronize false ;
    end ;

  if !small <> ""
  then
    begin
      Pip.pip !silent !big !small ;
      if not !silent
      then read_line () |> ignore ;
    end ;
      
  if !recursivity > 0
  then
    begin
      Pip.pip_n !silent !recursivity !big ;
      if not !silent
      then read_line () |> ignore
    end
