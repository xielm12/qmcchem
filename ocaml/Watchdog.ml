open Core.Std;;

let _list    = ref [] ;;
let _running = ref false;;
let _threads = ref [] ;;

(** Kill the current process and all children *)
let kill () =
  let kill pid = 
    Signal.send_i Signal.int (`Pid pid);
    Printf.printf "Killed %d\n" (Pid.to_int pid)
  in
  List.iter ~f:kill (!_list);
  exit 1 
;;


(** Start watchdog *)
let start () =

  if (!_running) then
    failwith "Watchdog error: Already running" 
  else
    begin
      _running := true;

      let pause () =
        Time.Span.of_sec 1.
        |> Time.pause 
      in

      let pid_is_running pid =
         match (Sys.file_exists ("/proc/"^(Pid.to_string pid)^"/stat")) with
         | `No | `Unknown ->  false
         | `Yes -> true
      in

      let f () =
        while (!_running)
        do
          pause () ;

(*DEBUG
 List.iter (!_list) ~f:(fun x -> Printf.printf "%d\n%!" (Pid.to_int x));
 *)

          let continue () =
            List.fold_left (!_list) ~init:true ~f:(
              fun accu x -> accu && (pid_is_running x)
              ) 
          in
          if ( not (continue ()) ) then
            kill ()
        done
      in
      _threads := ( (Thread.create f) () ) :: (!_threads)
    end
;;

(** Stop watchdog *)
let stop () =
  if (!_running) then
    _running := false
  else
    failwith "Watchdog error: Already stopped"
;;

(** Add a PID to tracking *)
let add pid = 
  if (not !_running) then
    start ();
  _list := pid :: (!_list)
;;

(** Remove a PID from tracking *)
let del pid = 
  let rec aux accu = function 
    | [] -> accu
    | a :: rest ->
        if (a <> pid) then
          aux (a::accu) rest
        else
          aux accu rest
  in
  _list := aux [] (!_list);

  match (!_list) with
  | [] -> if (!_running) then stop ()
  | _ -> ()
;;

(** Fork and exec a new process *)
let fork_exec ~prog ~args () =
  let pid = 
    Unix.fork_exec ~prog ~args ()
  in

  let f () = 
    add pid;
    let success = 
      match (Unix.waitpid pid) with
      | Core_kernel.Std.Result.Ok () -> true
      | Core_kernel.Std.Result.Error (`Exit_non_zero n) ->
          ( Printf.printf "PID %d exited with code %d\n%!"
              (Pid.to_int pid) n ;
            false )
      | Core_kernel.Std.Result.Error (`Signal n) ->
          ( Printf.printf "PID %d killed with signal %d (%s)\n%!"
              (Pid.to_int pid) (Signal.to_system_int n)
              (Signal.to_string n) ;
            false )
    in
    del pid ;
    if (not success) then
      kill ()
  in
  _threads := ( (Thread.create f) () ) :: (!_threads);
  pid
;;

(** Wait for threads to finish *)
let join () =
(*  if (!_running) then stop (); *)
  List.iter ~f:Thread.join (!_threads);
  assert (not !_running)
;;
