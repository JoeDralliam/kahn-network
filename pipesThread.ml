module I : Kahn.S =
struct
  type 'a process = unit -> 'a
  type 'a in_port = Unix.file_descr
  type 'a out_port = Unix.file_descr

  let new_channel () =
    Unix.pipe ()

  let put v outp =
    (fun () ->
      let msg = Marshal.to_bytes v [] in
      ignore (Unix.write outp msg 0 (Bytes.length msg)))

  let get inp =
    (fun () ->
      let header = Bytes.create Marshal.header_size in
      Unix.read inp header 0 Marshal.header_size |> ignore ;
      let sz = Marshal.data_size header 0 in

      let data = Bytes.create sz in
      Unix.read inp data 0 sz |> ignore ;
      Marshal.from_bytes (Bytes.cat header data) 0

    )

  let return v = (fun () -> v)

  let bind p1 f =
    (fun () -> f (p1 ()) ())

  let doco l =
    (fun () ->
      let rec next l =
	match l with
	| [] -> ()
	| x :: t ->
	   begin
	     let th = Thread.create x () in
	     (next t; Thread.join th)
	   end
      in next l)
  let run f = f ()
end
