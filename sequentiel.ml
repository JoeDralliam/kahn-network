
module I : Kahn.S =
  struct
    module Scheduler :
    sig
      type t

      val schedule : unit -> t
      val add : (unit -> t) -> unit
    end =
    struct
      type t = unit

      let processes : (unit -> t) Queue.t = Queue.create ()

      let schedule () =
	if Queue.is_empty processes
	then ()
	else Queue.pop processes ()

      let add f = Queue.push f processes
    end

    type 'a process = ('a -> Scheduler.t) -> Scheduler.t
    type 'a in_port = 'a Queue.t
    type 'a out_port = 'a Queue.t

    let new_channel () =
      let q = Queue.create () in (q, q)

    let put v op =
      fun f ->
      Scheduler.add (fun () -> Queue.push v op ; f ()) ;
      Scheduler.schedule ()

    let rec get ip =
      fun f ->
	Scheduler.add
	  (fun () ->
	    if Queue.is_empty ip
	    then get ip f
	    else f (Queue.pop ip)) ;
	Scheduler.schedule ()


    let return a = (fun f -> f a)
    let bind p app = (fun f -> p (fun a -> (app a) f))

    let doco pl =
      fun f ->
      begin
	List.iter (fun p -> Scheduler.add (fun () -> p (Scheduler.schedule))) pl ;
	ignore (Scheduler.schedule ()) ;
	f ()
      end

    let run p =
      let res = ref (Obj.magic ()) in
      p (fun x -> (res := x ; Scheduler.schedule ())) ; !res
  end
