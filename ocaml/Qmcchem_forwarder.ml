open Core.Std;;

let bind_socket ~socket_type ~socket ~address =
  let rec loop = function 
  | 0 -> failwith @@ Printf.sprintf 
        "Unable to bind the forwarder's %s socket : %s\n"
        socket_type address
  | -1 -> () 
  | i ->  
      try 
        ZMQ.Socket.bind socket address; 
        loop (-1) 
      with 
      | Unix.Unix_error _ -> (Time.pause @@ Time.Span.of_float 1. ; loop (i-1) ) 
      | other_exception -> raise other_exception 
  in loop 10 



let run ezfio_filename dataserver =

  let dataserver_address, dataserver_port =
    Substring.create ~pos:6 dataserver
    |> Substring.to_string
    |> String.lsplit2_exn ~on:':' 
  and qmc = 
    Lazy.force Qmcchem_config.qmc
  in

  (* Go into /dev/shm *)
  Unix.chdir Qmcchem_config.dev_shm;

  let tmpdir = 
    ezfio_filename ^ "_" ^ dataserver_port
  in

  (* Port of the data server *)
  let port = 
    (Int.of_string dataserver_port)+10
  in

  (* Build qmc executable command *)
  let prog, args = 
      qmc, 
    [ qmc ; ezfio_filename ;
       Printf.sprintf "ipc://%s:%d" Qmcchem_config.dev_shm port ];
  in

  (* Create the temporary directory. If it is possible, then the process is a
   * master and the forwarder will start. Otherwise, only start a qmc process.
   *)
  let () = 
    try
      Unix.mkdir tmpdir;
      Unix.chdir tmpdir
    with
    | Unix.Unix_error _ ->
        begin
          Unix.chdir tmpdir;
          Time.pause @@ Time.Span.of_float 0.1;
          match (Sys.file_exists "PID") with
          | `No 
          | `Unknown -> ()
          | `Yes ->
              let pid = 
                In_channel.with_file "PID" ~f:(fun ic ->
                  match (In_channel.input_line ic) with
                  | Some x -> x
                  | None -> "-1" )
                |> Int.of_string
              in
              match pid with
              | -1 -> ()
              | pid ->
                  begin
                    match Signal.send (Signal.of_system_int 0) (`Pid (Pid.of_int pid)) with
                    | `No_such_process ->  ()
                    | _ -> ignore @@ Unix.exec ~prog ~args ()
                  end
        end
  in

  (* Now, only one forwarder will execute the following code *)
  Out_channel.with_file "PID" ~f:(fun oc ->
    Unix.getpid ()
    |> Pid.to_int
    |> Printf.sprintf "%d\n" 
    |> Out_channel.output_string oc);

  (* Fork a qmc *)
  ignore @@
    Watchdog.fork_exec ~prog ~args ();

  (* If there are MICs, use them here (TODO) *)
  
  (* Fetch input *)
  let zmq_context =
    ZMQ.Context.create ()
  in

  let terminate () = 
    (* Clean up the temp directory *)
    Unix.chdir Qmcchem_config.dev_shm;
    let command = 
      Printf.sprintf "rm -rf -- \"%s\" " tmpdir
    in
    match Unix.system command with
    | Ok _ -> ()
    | _    -> print_endline "Unable to remove temporary directory"
    ;
    ZMQ.Context.terminate zmq_context ;
    for i=port to port+4
    do
      let filename =
         Filename.concat Qmcchem_config.dev_shm  (Printf.sprintf ":%d" i)
      in
      try
         Unix.unlink filename
      with
      | _ ->  ()
      ;
    done;
    Watchdog.kill ()
  in


  (* Signal handler to Kill properly all the processes *)
  let handler s =
    Printf.printf "Forwarder received the %s signal... killing\n%!" (Signal.to_string s);
    terminate ();
  in
  List.iter [
     Signal.int  ;
     Signal.term ;
     Signal.quit ;
    ]
    ~f:(fun x -> Signal.Expert.handle x handler)
  ;


  (* Fetch walkers *)
  let walk_num =
    ref 0
  and walkers = 
    ref []
  in


  (* Status thread *)
  let status = 
    ref Status.Running
  in

  let start_status_thread =

    let f () =
      let pub_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.pub
      and address =
        Printf.sprintf "ipc://%s:%d" Qmcchem_config.dev_shm (port+1);
      in
      bind_socket "PUB" pub_socket address;

      let sub_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.sub
      and address =
        Printf.sprintf "tcp://%s:%d" dataserver_address (port+1-10)
      in
      ZMQ.Socket.connect sub_socket address;
      ZMQ.Socket.subscribe sub_socket "";

      let pollitem =
        ZMQ.Poll.mask_of
        [| (sub_socket, ZMQ.Poll.In) ;
        |]
      in

      while (!status <> Status.Stopped)
      do
        let polling =
          ZMQ.Poll.poll ~timeout:1000 pollitem
        in
        if (polling.(0) = Some ZMQ.Poll.In) then
          begin
            let msg =
              ZMQ.Socket.recv ~block:false sub_socket
            in
            ZMQ.Socket.send pub_socket msg;
            status := Status.of_string msg;
          end;
      done;
      List.iter ~f:(fun socket ->
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket)
        [ sub_socket ; pub_socket ]
    in
    Thread.create f
  in

  let start_log_thread =

    let f () =
      let sub_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.xsub
      and address =
        Printf.sprintf "ipc://%s:%d" Qmcchem_config.dev_shm (port+3);
      in
      bind_socket "XSUB" sub_socket address;

      let pub_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.xpub
      and address =
        Printf.sprintf "tcp://%s:%d" dataserver_address (port+3-10)
      in
      ZMQ.Socket.connect pub_socket address;

      let pollitem =
        ZMQ.Poll.mask_of
        [| (sub_socket, ZMQ.Poll.In) ;
           (pub_socket, ZMQ.Poll.In) ;
        |]
      in

      (* Main loop *)
      while (!status <> Status.Stopped)
      do
        let polling =
          ZMQ.Poll.poll ~timeout:1000 pollitem
        in
        if (polling.(0) = Some ZMQ.Poll.In) then
          begin
            ZMQ.Socket.recv ~block:false sub_socket
            |> ZMQ.Socket.send pub_socket ;
          end
        else if (polling.(1) = Some ZMQ.Poll.In) then
          begin
            Printf.eprintf "Forwarder subscribe\n%!";
            ZMQ.Socket.recv ~block:false pub_socket
            |> ZMQ.Socket.send sub_socket ;
          end
      done;
      List.iter ~f:(fun socket ->
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket)
        [ sub_socket ; pub_socket ]
    in
    Thread.create f
  in

  (* Proxy thread *)
  let start_proxy_thread =
    let f () =

      let req_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.req
      in
      ZMQ.Socket.connect req_socket dataserver;
      ZMQ.Socket.set_receive_timeout req_socket 600_000;

      let dealer_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.dealer
      in

      bind_socket "PROXY" dealer_socket "inproc://dealer";
      ZMQ.Socket.set_receive_high_water_mark dealer_socket 100_000;
      ZMQ.Socket.set_send_high_water_mark dealer_socket 100_000;
      ZMQ.Socket.set_immediate dealer_socket true;
      ZMQ.Socket.set_linger_period dealer_socket 600_000;

      let fetch_walkers () =
        ZMQ.Socket.send_all req_socket ["get_walkers" ; Int.to_string !walk_num ];
        ZMQ.Socket.recv_all req_socket
      in

      let pollitem =
        ZMQ.Poll.mask_of
        [| (dealer_socket, ZMQ.Poll.In) ;
        |]
      in

      (* EZFIO Cache *)
      let ezfio_cache = 
        String.Table.create ()
      in
      let handle_ezfio msg =
        match Hashtbl.find ezfio_cache msg with
        | Some result -> result
        | None ->
          begin
            ZMQ.Socket.send_all req_socket ["Ezfio" ; msg];
            let result = 
              ZMQ.Socket.recv_all req_socket
            in
            match (Hashtbl.add ezfio_cache ~key:msg ~data:result) with
            | `Ok -> result
            | `Duplicate -> result
          end
      in
        

      (* Main loop *)
      while (!status <> Status.Stopped)
      do
        let polling =
          ZMQ.Poll.poll ~timeout:1000 pollitem
        in
        if (polling.(0) = Some ZMQ.Poll.In) then
          begin
            let raw_msg = 
              ZMQ.Socket.recv_all ~block:false dealer_socket
            in
            let header, msg =
              let rec aux header = function
                | ""   :: msg  -> List.rev ("" :: header), Message.create msg
                | head :: tail -> aux (head::header) tail
                | _            -> failwith "Too many routers in the middle"
              in
              aux [] (List.map ~f:String.strip raw_msg)
            in
            let handle message =
              match message with
              | Message.Ezfio ezfio_msg -> 
                  let result = 
                    handle_ezfio ezfio_msg
                  in
                  ZMQ.Socket.send_all dealer_socket (header @ result) 
              | Message.GetWalkers n_walks ->
                begin
                  if (!walk_num = 0) then
                    begin
                      walk_num := Qptypes.Strictly_positive_int.to_int n_walks;
                      walkers := fetch_walkers ();
                    end;
                  ZMQ.Socket.send_all dealer_socket (header @ !walkers);
                  walkers := fetch_walkers ();
                end
              | Message.Test ->
                  ZMQ.Socket.send_all dealer_socket (header @ [ "OK" ])
              | Message.Error _ ->  ()
              | Message.Register _
              | Message.Unregister _ 
              | Message.Walkers  _
              | Message.Property _ ->
                  failwith "Bad message"
            in handle msg
          end;
      done;
      ZMQ.Socket.set_linger_period dealer_socket 1000 ;
      ZMQ.Socket.set_linger_period req_socket 1000 ;
      ZMQ.Socket.close dealer_socket;
      ZMQ.Socket.close req_socket;
    in
    Thread.create f
  in

  (* Main thread *)
  let start_main_thread =
    let f () =

      let dealer_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.dealer
      in
      ZMQ.Socket.connect dealer_socket dataserver;
      ZMQ.Socket.set_linger_period dealer_socket 600_000;

      let proxy_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.dealer
      in
      ZMQ.Socket.connect proxy_socket "inproc://dealer";

      let router_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.router
      and address =
        Printf.sprintf "ipc://%s:%d" Qmcchem_config.dev_shm (port);
      in
      bind_socket "ROUTER" router_socket address;
      ZMQ.Socket.set_receive_high_water_mark router_socket 100000;
      ZMQ.Socket.set_send_high_water_mark router_socket 100000;
      ZMQ.Socket.set_immediate router_socket true;
      ZMQ.Socket.set_linger_period router_socket 600_000;

      (* Pull socket for computed data *)
      let push_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.push
      and address =
        Printf.sprintf "tcp://%s:%d" dataserver_address (port+2-10)
      in
      ZMQ.Socket.connect push_socket address;
      ZMQ.Socket.set_linger_period push_socket 600_000;

      let pull_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.pull
      and address =
        Printf.sprintf "ipc://%s:%d" Qmcchem_config.dev_shm (port+2);
      in
      bind_socket "PULL" pull_socket address;


      (* Handles messages coming into the ROUTER socket. *)
      let handle_router () =
        let raw_msg = 
          ZMQ.Socket.recv_all ~block:false router_socket
        in
        let header, msg =
          let rec aux header = function
            | ""   :: msg  -> List.rev ("" :: header), Message.create msg
            | head :: tail -> aux (head::header) tail
            | _            -> failwith "Too many routers in the middle"
          in
          aux [] (List.map ~f:String.strip raw_msg)
        in
        let handle message =
          match message with
          | Message.GetWalkers _ 
          | Message.Ezfio _
          | Message.Test ->
              ZMQ.Socket.send_all proxy_socket raw_msg
          | Message.Register _
          | Message.Unregister _ ->
              ZMQ.Socket.send_all dealer_socket raw_msg
          | Message.Walkers (_, _, _)
          | Message.Property _ ->
              failwith "Bad message"
          | Message.Error _ -> ()
        in handle msg
      in

      let handle_dealer () =
        ZMQ.Socket.recv_all ~block:false dealer_socket
        |> ZMQ.Socket.send_all router_socket
      in

      let handle_proxy () =
        ZMQ.Socket.recv_all ~block:false proxy_socket
        |> ZMQ.Socket.send_all router_socket
      in

      let select_n_of ~n ~len l =
        let a = 
          Array.of_list l
        in
        let s = 
          (Array.length a)/ len
        in
        let fetch i = 
          let rec loop accu = function 
          | -1 -> accu
          | k -> loop ((Array.get a (i+k)) :: accu) (k-1)
          in
          loop [] (len-1)
        in
        let rec select accu = function
        | 0 -> accu
        | i -> let new_accu =
                (fetch @@ Random.int s) :: accu
              in 
              select new_accu (i-1)
        in
        select [] n
        |> List.concat
      in

      (* Handles messages coming into the PULL socket. *)
      let handle_pull () =
        let message = 
          ZMQ.Socket.recv_all ~block:false pull_socket
        in
        let new_message = 
          match message with
          | "elec_coord":: hostname :: pid :: id :: n_str :: rest ->
            let n = 
              Int.of_string n_str
            in
            let len =
              n / !walk_num
            in
            if (n < 5*len) then
              message
            else
              List.concat [ [ "elec_coord" ; hostname ; pid ; id ; 
              Int.to_string (5*len)] ; ( select_n_of ~n:5 ~len rest ) ]
          | _ -> message
        in
        ZMQ.Socket.send_all  push_socket  new_message
      in

      (* Polling item to poll ROUTER and PULL sockets. *)
      let pollitem =
        ZMQ.Poll.mask_of
        [| (router_socket , ZMQ.Poll.In) ;
            (pull_socket  , ZMQ.Poll.In) ;
            (dealer_socket, ZMQ.Poll.In) ;
            (proxy_socket , ZMQ.Poll.In)
        |]
      in
      (* Main loop *)
      while (!status <> Status.Stopped)
      do
        let polling =
          ZMQ.Poll.poll ~timeout:1000 pollitem
        in
        if (polling.(0) = Some ZMQ.Poll.In) then
          handle_router ();
        if (polling.(1) = Some ZMQ.Poll.In) then
          handle_pull ();
        if (polling.(2) = Some ZMQ.Poll.In) then
          handle_dealer ();
        if (polling.(3) = Some ZMQ.Poll.In) then
          handle_proxy ();
      done;
      List.iter ~f:(fun socket ->
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket)
      [ router_socket ; dealer_socket ; push_socket ; pull_socket ; proxy_socket ]
    in
    Thread.create f
  in


  (* Start the status thread and the main thread *)
  begin
    try
      (List.iter ~f:Thread.join 
        [ start_status_thread ();
          start_log_thread ();
          start_proxy_thread ();
          start_main_thread ();
        ])
    with
    | err ->
      begin
        print_endline "Trapped error. Waiting 10 seconds...";
        status := Status.Stopping;
        Time.Span.of_sec 10. |> Time.pause;
        raise err
      end
  end;

  (* Wait for the qmc process to complete *)
  try
    ignore (Watchdog.join ());
    terminate ()
  with
  | error -> 
    begin
      terminate ();
      raise error
    end

