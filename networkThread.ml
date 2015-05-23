module I : Kahn.S =
struct
  type 'a process = unit -> 'a
  type 'a in_port = Unix.file_descr
  type 'a out_port = Unix.file_descr

  let port = ref 8081

  let new_channel () =
    let open Unix in
    let addr = ADDR_INET (inet_addr_loopback, !port) in
    port := !port + 1 ;
    let in_sock = socket (domain_of_sockaddr addr) SOCK_STREAM 0 in
    let out_sock = socket (domain_of_sockaddr addr) SOCK_STREAM 0 in

    bind out_sock addr ;
    listen out_sock 1 ;

    connect in_sock addr ;

    let (out_sock, _) = accept out_sock in
    (in_sock, out_sock)


  let put v outp =
    (fun () ->
      let msg = Marshal.to_bytes v [] in
      ignore (Unix.send outp msg 0 (Bytes.length msg) []))

  let get inp =
    (fun () ->
      let header = Bytes.create Marshal.header_size in
      Unix.recv inp header 0 Marshal.header_size [] |> ignore ;
      let sz = Marshal.data_size header 0 in
      let data = Bytes.create sz in
      Unix.recv inp data 0 sz [] |> ignore ;
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
	     next t ; Thread.join th
	   end
      in next l)

  let run f = f ()


end
