open Core.Std

let full_run ?(start_dataserver=true) ezfio_filename  = 
  (* Identify the job scheduler *)
  let launcher =
    Launcher.find ()
  and scheduler = 
    Scheduler.find ()
  in
  Printf.printf "Scheduler : %s\n" (Scheduler.to_string scheduler);
  Printf.printf "Launcher  : %s\n" (Launcher.to_string  launcher );


  (* Create the node file *)
  let server_file = 
    Filename.concat ezfio_filename "nodefile"
  in
  Out_channel.with_file server_file ~f:(fun out_channel ->
    Launcher.create_nodefile ()
    |> Out_channel.output_string out_channel 
  ) ;
  

  (* Get the configuration of executables *)
  let qmcchem =
    Lazy.force Qmcchem_config.qmcchem
  and qmc =
        [ Lazy.force Qmcchem_config.qmcchem ; "run" ; "-q" ]
  in


  if (start_dataserver) then
    begin
      (* Reset socket address in EZFIO *)
      Ezfio.set_simulation_http_server "tcp://localhost:65534";


      (* Start the data server *)
      let prog, args = 
        qmcchem,  [ qmcchem; "run" ; "-d" ; ezfio_filename]
      in
      let pid_dataserver = 
        Watchdog.fork_exec ~prog ~args ()
      in
      Printf.printf "%7d : %s\n%!" (Pid.to_int pid_dataserver) (String.concat ~sep:" " args)
    end;
  

  (* Check if the ZMQ Rep socket is open *)
  let test_open_rep_socket () =
    let zmq_context =
      ZMQ.Context.create ()
    in
    let socket = 
      ZMQ.Socket.create zmq_context ZMQ.Socket.req
    and address = 
      Ezfio.get_simulation_http_server ()
    in
    let reply = 
       try
        (
           ZMQ.Socket.set_receive_timeout socket 100;
           ZMQ.Socket.connect socket address;
           ZMQ.Socket.send socket (Message.(to_string Test));
           ZMQ.Socket.recv socket
         ) with
       | Unix.Unix_error _ ->
           begin
             ZMQ.Socket.set_linger_period socket 1 ;
             ZMQ.Socket.close socket;
             ZMQ.Context.terminate zmq_context;
             "Failed"
           end
    in
    reply = "OK"
  in


  (* Wait until the rep socket is open *)
  let rec count = function 
  | 0  -> false
  | -1 -> true
  | n  -> 
    if (not (test_open_rep_socket ())) then
      begin
       Time.pause (Time.Span.of_float 0.5);
       count (n-1);
      end
    else
      count (-1);
  in
  if (not (count 300)) then
    Watchdog.kill ();
     

  (* Start the qmc processes *)
  let prog, args =
    let launcher =
      Launcher.(find () |> to_string)
    in
    match launcher
    |> String.split ~on:' '
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun x -> x <> "")
    with
    | launcher_exe::launcher_flags -> 
       launcher_exe, launcher_exe :: launcher_flags @ qmc @ [ 
         Ezfio.get_simulation_http_server () ; ezfio_filename ] 
    | _ -> failwith "Error in launcher"
  in
  let pid_qmc = 
    try 
      Watchdog.fork_exec ~prog ~args ()
    with
    | Unix.Unix_error _ ->
        begin
          let command = 
            String.concat ~sep:" " args 
          in
          Printf.printf "
============================================================
Error: Unable to run the following command
 %s
============================================================
\n%!" command ;
          Watchdog.kill () 
        end
  in
  Printf.printf "%7d : %s\n%!" (Pid.to_int pid_qmc) (String.concat ~sep:" " args);  

  (* Wait for processes to finish *)
  Watchdog.join ()


let data_run ezfio_filename =
  Qmcchem_dataserver.run ezfio_filename ~daemon:false

let qmc_run dataserver ezfio_filename =
  Qmcchem_forwarder.run ezfio_filename dataserver

let ssh_run host dataserver ezfio_filename =
  print_endline ("ssh "^host^" "^ezfio_filename^" "^dataserver)

let run a d ?q ?s ezfio_filename = 

  Ezfio.set_file ezfio_filename;
  let ezfio_filename = 
    Lazy.force Qputils.ezfio_filename
  in

  (* Signal handler to Kill properly all the processes *)
  let handler s = 
    Printf.printf "Received the %s signal... killing\n" (Signal.to_string s);
    Watchdog.kill ();
  in
  List.iter [
     Signal.term ;
     Signal.quit ;
     Signal.int  
    ]
    ~f:(fun x -> Signal.Expert.handle x handler)
  ;

  (* Validate input *)
  Input.validate ();
(*  Printf.printf "MD5 : %s\n" (Lazy.force Md5.hash) ; *)

  let runtype = 
    match (a,d,q,s) with
    | (false,false, None, None) -> `Run
    | (false,true, None, None) -> `Data
    | (true,false, None, None) -> `Add 
    | (false,false, Some dataserver, None) -> `Qmc dataserver
    | (false,false, Some dataserver, Some host) -> `Ssh (host, dataserver)
    | _ -> failwith "Options (-a|-d|-q [-s]) are mutually exclusive"
  in

  let run = 
    match runtype with
    | `Run  -> full_run ~start_dataserver:true
    | `Data -> data_run 
    | `Add  -> full_run ~start_dataserver:false
    | `Qmc dataserver -> qmc_run dataserver
    | `Ssh (host,dataserver)  -> ssh_run host dataserver
  in
  run ezfio_filename




let spec =
  let open Command.Spec in
  empty
  +> flag "a" no_arg
     ~doc:(" Add more resources to a running calculation.")
  +> flag "d" no_arg
     ~doc:(" Start a dataserver process on the local host.")
  +> flag "q" (optional string)
     ~doc:("<dataserver_addr> Start a qmc process on the local host.")
  +> flag "s" (optional string)
     ~doc:("<host> Start a qmc process on <host>.")
  +> anon ("ezfio_file" %: string)



let command =
    Command.basic
    ~summary: "Run a calculation"
    ~readme:(fun () ->
      "
Run QMC=Chem
      ")
    spec
    (fun a d q s ezfio_file () -> run a d ?q ?s ezfio_file )



