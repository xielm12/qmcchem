open Core.Std
open Qptypes

(** Data server of QMC=Chem.

5 ZeroMQ sockets are opened:
- a REP socket used for registering/unregisterning the walkers and for the clients to fetch the initial walkers positions
- a PULL socket to pull the results computed by the clients
- a PUB socket to send the status to the clients for the termination
- a XSUB socket for receiving debug
- a XPUB socket for sending debug

@author A. Scemama
*)

let initialization_timeout = 600.

let bind_socket ~socket_type ~socket ~address =
  try
    ZMQ.Socket.bind socket address
  with 
  | Unix.Unix_error (_, message, f) -> 
    failwith @@ Printf.sprintf 
        "\n%s\nUnable to bind the dataserver's %s socket :\n %s\n%s"
        f socket_type address message
  | other_exception -> raise other_exception


let run ?(daemon=true) ezfio_filename =

  Ezfio.set_file ezfio_filename ;

  (** Measures the time difference between [t0] and [Time.now ()] *)
  let delta_t t0 =
    let t1 =
      Time.now ()
    in
    Time.abs_diff t1 t0
  in


  (** {2 ZeroMQ initialization} *)

  let zmq_context =
    ZMQ.Context.create ()
  in



  (** Maximum size of the blocks file before compressing *)
  let max_file_size = ref (
    Byte_units.create `Kilobytes 64.)
  in


  let hostname =
    Lazy.force Qmcchem_config.hostname
  in

  (** Status variable (mutable) *)
  let status = 
    ref Status.Queued
  in

  let change_status s =
    status := s;
    Status.write s;
    Printf.printf "Status : %s\n%!" (Status.to_string s)
  in
  change_status Status.Queued;



  (*
  (** Checks if the port is already open (not working properly yet) *)
  let check_port n =
    let adress_prefix =
      "tcp://*:"
    in
    let result = 
      List.fold [0;1;2;3] ~init:true ~f:(fun accu i ->
        let address =
          adress_prefix ^ (Int.to_string (n+i))
        in
        let socket =
          ZMQ.Socket.create zmq_context ZMQ.Socket.rep
        in
        let result = 
          try
            (ZMQ.Socket.bind socket address; accu );
          with
          | _ -> false;
        in
        ZMQ.Socket.close socket; 
        result
      )
    in
    if (result) then
        `Available
    else
        `Unavailable
  in
  *)



  (** Random port number between 49152 and 65535 *)
  let port =
    let newport = 
  (*    ref (49152 + (Random.int 16383)) *)
      ref ( 1024 + (Random.int (49151-1024)))
    in
  (*
    while ((check_port !newport) = `Unavailable)
    do
      newport := 49152 + (Random.int 16383)
    done;
  *)
    !newport
  in


  let debug_socket = 
    ZMQ.Socket.create zmq_context ZMQ.Socket.xpub
  and address =
    Printf.sprintf "tcp://*:%d" (port+4)
  in
  bind_socket "XPUB" debug_socket address;

  let close_debug_socket () =
      ZMQ.Socket.set_linger_period debug_socket 1000 ;
      ZMQ.Socket.close debug_socket 
  in

  (** Sends a log text to the debug socket *)
  let send_log socket size t0 text =
    let dt = 
      delta_t t0
    in
    let message = 
      Printf.sprintf "%20s : %8d : %10s : %s" 
        socket size text (Time.Span.to_string dt) 
    in
    ZMQ.Socket.send debug_socket message
  in


  (** {2 Walkers} *)

  (** Number of electrons *)
  let elec_num = 
    Lazy.force Qputils.elec_num
  in


  (** Size of the walkers message *)
  let walkers_size =
    20*3*(elec_num+1)
  in

  (** Seconds after when the block is ended on the worker. *)
  let block_time =
    Input.Block_time.read ()
    |> Input.Block_time.to_float
  in

  (** Total number of walkers to keep for restart *)
  let walk_num_tot =
    Input.Walk_num_tot.read ()
  in

  (** Array of walkers. The size is [walk_num_tot]. *)
  let walkers_array = 
    let t0 = 
      Time.now ()
    in
    let j =
      3*elec_num + 3
    in
    let result = 
      if ( not(Ezfio.has_electrons_elec_coord_pool ()) ) then
        begin
          Printf.printf "Generating initial walkers...\n%!";
          Unix.fork_exec ~prog:(Lazy.force Qmcchem_config.qmc_create_walkers)
              ~args:["qmc_create_walkers" ; ezfio_filename] ()
          |> Unix.waitpid_exn ;
          Printf.printf "Initial walkers ready\n%!"
        end ;

      let size =
        Ezfio.get_electrons_elec_coord_pool_size ()
      and ez =
        Ezfio.get_electrons_elec_coord_pool ()
        |> Ezfio.flattened_ezfio
        |> Array.map ~f:Float.to_string
      in
      try
        Array.init walk_num_tot ~f:(fun i ->
          Array.sub ~pos:(j*(i mod size)) ~len:j ez)
      with
      | Invalid_argument _ ->
          failwith "Walkers file is broken."
    in
    String.concat [ "Read " ; Int.to_string (Array.length result) ;
    " walkers"] 
    |> send_log "status" 0 t0 ;
    result
  in


  (** Id of the last saved walker (mutable). *)
  let last_walker =
    ref 0
  in


  (** Last time when the walkers were saved to disk. *)
  let last_save_walkers = 
    ref (Time.now ())
  in


  (** Saves the walkers to disk. *)
  let save_walkers () =
    if (delta_t !last_save_walkers > (Time.Span.of_sec 10.) ) then
      begin
        let t0 =
          Time.now ()
        in
        Ezfio.set_electrons_elec_coord_pool_size walk_num_tot ;
        let walkers_list = 
          Array.map walkers_array ~f:Array.to_list
          |> Array.to_list
          |> List.concat
          |> List.map ~f:Float.of_string
        in
        Ezfio.set_electrons_elec_coord_pool (Ezfio.ezfio_array_of_list
          ~rank:3 ~dim:[| elec_num+1 ; 3 ; walk_num_tot |] ~data:walkers_list);
        send_log "status" walk_num_tot t0 "Saved walkers";
        last_save_walkers := Time.now ();
      end
  in



  (** Increments the [last_walker] mutable value, and saves the walkers to
      disk if the array of walkers is filled. In that case, sets the last_walker to 0.
  *)
  let increment_last_walker () =
    last_walker := !last_walker + 1;
    if (!last_walker = walk_num_tot) then
      begin
        last_walker := 0 ;
        save_walkers ()
      end
  in



  (** {2 Set of workers} *)

  (** A hash table is kept to track the running workers. The keys are the 
      built as string containing the couple ([Compute_node], [PID]), and
      the values are the last communication time.
  *)



  (** The hash table for workers *)
  let workers_hash = 
    String.Table.create ()
  in



  (** Creates a key using the couple ([Compute_node], [PID]) *)
  let key compute_node pid = 
    String.concat [ 
      (Compute_node.to_string compute_node); " "; 
      (Pid.to_string pid) ]
  in



  (** Adds a new worker to the hash table.
      @raise Failure when the worker is already in the table. *)
  let add_worker w pid =
    let s =
      key w pid
    in
    match Hashtbl.add workers_hash ~key:s ~data:(Time.now ()) with
    | `Ok        -> ()
    | `Duplicate -> failwith (s^" already registered")
  in



  (** Deletes a new worker from the hash table.
      @raise Failure when the worker is not in the table. *)
  let del_worker w pid =
    let s =
      key w pid
    in
    match Hashtbl.find workers_hash s with
    | Some x -> Hashtbl.remove workers_hash s
    | None   -> failwith (s^" not registered") 
  in



  (** Sets the last access of the worker to [Time.now ()] *)
  let touch_worker w pid = 
    let s = 
      key w pid
    in
    Hashtbl.set workers_hash ~key:s ~data:(Time.now ())
  in


  (** Returns the number of connected workers *)
  let n_connected hash now =
    let delta =
      Time.Span.of_sec (initialization_timeout +. block_time *. 2.)
    in
    Hashtbl.filter hash ~f:(fun x -> (Time.abs_diff now x) <= delta)
    |> Hashtbl.length
  in




  (** Current PID. *)
  let dataserver_pid =
    Unix.getpid ()
  in


  (** Name of the blocks file written by the current process. *)
  let block_channel_filename = 
    let dirname = 
      Lazy.force Block.dir_name
    in
    let () = 
      match Sys.is_directory dirname with
        | `Yes -> ()
        | _    -> Unix.mkdir_p dirname
    in 
    Filename.concat dirname (
      hostname  ^ "." ^ (Pid.to_string dataserver_pid)
    )
  in

  (** Name of the blocks file written by the current process, currently locked *)
  let block_channel_filename_locked =
    block_channel_filename ^ ".locked"
  in

  let block_channel_filename_tmp =
    block_channel_filename ^ ".tmp"
  in


  (** [Out_channel] corresponding to the blocks file written by the current process. *)
  let block_channel =
    try
      ref (Out_channel.create block_channel_filename_locked)
    with
    | Sys_error _ ->
        begin
          (* NFS Stale file handle :
           * Wait 5 seconds, and retry *)
          Time.Span.of_sec 5. |> Time.pause;
          ref (Out_channel.create block_channel_filename_locked)
        end
  in


  (** Compresses the blocks file by merging all blocks with the same block ID and the
      same host name, but different PIDs. The result is merging all the CPU cores of
      the compute nodes. Happens when [max_file_size] is reached.
  *)
  let compress_block_file filename =
    let t0 = 
      Time.now ()
    in
    Out_channel.close !block_channel;
    Unix.rename ~src:block_channel_filename_locked ~dst:block_channel_filename_tmp;
    Random_variable.compress_files ();
    send_log "status" 0 t0 "Compressed block file";
    block_channel := Out_channel.create ~append:true block_channel_filename_locked
  in



  (** {2 Threads} *)

  (** {3 Status thread} *)

  let start_status_thread = 
      let t0 =
        Time.now ()
      in
      Thread.create (fun () -> 
        send_log "status" 0 t0 "Starting status thread";

        let socket =
          ZMQ.Socket.create zmq_context ZMQ.Socket.pub
        and address = 
          Printf.sprintf "tcp://*:%d" (port+1)
        in
        bind_socket "PUB" socket address;
        let delay =
          Time.Span.of_ms 300.
        and delay_read =
          Time.Span.of_sec 2.
        in

        let start_time = 
          Time.now ()
        and stop_time = 
          ref (Time.Span.of_sec Input.Stop_time.(read () |> to_float) )
        in
        
        let last_update =
          ref start_time
        in     

        while (!status <> Status.Stopped)
        do
          Time.pause delay;
          let now =
            Time.now ()
          in
          let status_string = 
            Status.to_string !status
          in
          ZMQ.Socket.send socket status_string;
          send_log "status" (String.length status_string) now status_string;

          let test = 
            if (Time.abs_diff now !last_update > delay_read) then
              let n_connect = 
                n_connected workers_hash now 
              in
              `Update n_connect
            else if (Time.abs_diff now start_time > !stop_time) then
              `Terminate 
            else if (Time.abs_diff now start_time > Time.Span.of_sec initialization_timeout) then
              `Timeout 
            else
              `None 
          in

          match (daemon, !status, test) with
          | (_    , _              , `None      ) -> ()
          | (_    , Status.Running , `Terminate ) -> change_status Status.Stopping
          | (false, Status.Running , `Update 0  ) -> change_status Status.Stopped
          | (true , Status.Running , `Update 0  ) -> change_status Status.Queued 
          | (_    , _              , `Update i  ) -> 
              begin
                  status := Status.read ();
                  last_update := now;
                  stop_time := Time.Span.of_sec Input.Stop_time.(read () |> to_float) ;
                  let n_tot =
                    Hashtbl.length workers_hash 
                  in
                  if (i <> n_tot) then 
                    begin
                      Printf.sprintf "Connected workers : %d / %d" i n_tot
                      |> send_log "status" 0 now 
                    end
              end
          | (false, Status.Queued  , `Timeout   ) -> change_status Status.Stopped 
          | (_, _, _) ->  ()
          ;
        done;
        ZMQ.Socket.send socket (Status.to_string !status);
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket
      )
  in
    
  (** {3 Log thread} *)

  let start_log_thread = 
      let t0 =
        Time.now ()
      in
      Thread.create (fun () -> 
        send_log "status" 0 t0 "Starting log thread";

        let socket =
          ZMQ.Socket.create zmq_context ZMQ.Socket.xsub
        and address = 
          Printf.sprintf "tcp://*:%d" (port+3)
        in
        bind_socket "XSUB" socket address;

        let pollitem =
          ZMQ.Poll.mask_of 
          [| (socket , ZMQ.Poll.In) ;
             (debug_socket , ZMQ.Poll.In) 
          |]
        in
        while (!status <> Status.Stopped)
        do
          let polling =
            ZMQ.Poll.poll ~timeout:1000 pollitem 
          in
          if (polling.(0) = Some ZMQ.Poll.In) then
            begin
              let message = 
                ZMQ.Socket.recv_all ~block:false socket
                |> String.concat ~sep:" "
              in
              let now =
                Time.now ()
              in
              send_log "log" 0 now message
            end
          else if (polling.(1) = Some ZMQ.Poll.In) then
            begin
              (* Forward subscription from XPUB to XSUB *)
              ZMQ.Socket.recv_all ~block:false debug_socket
              |> ZMQ.Socket.send_all socket 
            end
        done;
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket
      )
  in
  (** {3 Main thread} *)

  let random_walkers n_walks = 
    let rec walkers accu = function
    | 0 -> accu 
    | n -> 
      let random_int =
        Random.int (Strictly_positive_int.to_int n_walks)
      in
      let new_accu = 
         walkers_array.(random_int) :: accu
      in
      walkers new_accu (n-1) 
    in
    walkers [] (Strictly_positive_int.to_int n_walks)
    |> Array.concat
    |> Array.to_list
  in

  let start_main_thread =
    let wall0 =
    Time.now ()
    in
    let f () = 
    
      change_status Status.Queued;
      send_log "status" 0 wall0 "Starting main thread";

      (** Reply socket *)
      let rep_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.rep
      and address = 
        Printf.sprintf "tcp://*:%d" port
      in
      bind_socket "REP" rep_socket address;
      ZMQ.Socket.set_receive_high_water_mark rep_socket 100000;
      ZMQ.Socket.set_send_high_water_mark rep_socket 100000;
      ZMQ.Socket.set_immediate rep_socket true; 

      (** EZFIO Cache *)
      let ezfio_cache =
        String.Table.create ()
      in
      let handle_ezfio msg =
        match Hashtbl.find ezfio_cache msg with
        | Some result -> result
        | None ->
          begin
            let result =
              decode_ezfio_message msg
            in
            match (Hashtbl.add ezfio_cache ~key:msg ~data:result) with
            | `Ok -> result
            | `Duplicate -> result
          end
      in

      (** Pull socket for computed data *)
      let pull_socket =
        ZMQ.Socket.create zmq_context ZMQ.Socket.pull
      and address = 
        Printf.sprintf "tcp://*:%d" (port+2)
      in
      bind_socket "PULL" pull_socket address;


      (** Address of the dataserver *)
      let server_address =
        let ip =
          Lazy.force Qmcchem_config.ip_address 
        in
        Printf.sprintf "tcp://%s:%d" ip port
      in
      Ezfio.set_simulation_http_server server_address;
      Printf.printf "Server address: %s\n%!" server_address;


      (** Polling item to poll REP and PULL sockets. *)
      let pollitem =
        ZMQ.Poll.mask_of 
        [| (  rep_socket, ZMQ.Poll.In) ;
           ( pull_socket, ZMQ.Poll.In) ;
        |]
      in


      (** Handles messages coming into the REP socket. *)
      let handle_rep () = 
        let raw_msg =
          ZMQ.Socket.recv_all ~block:false rep_socket
        in
        let t0 = 
          Time.now ()
        in
        let msg =
          List.map ~f:String.strip raw_msg
          |> Message.create 
        and msg_size =
          List.fold ~init:0 ~f:(fun accu x -> accu + (String.length x)) raw_msg
        in
        let handle = function
          | Message.Ezfio ezfio_msg -> 
              let result = 
                handle_ezfio ezfio_msg
              in
              ZMQ.Socket.send_all rep_socket 
                [ String.length result
                  |> Printf.sprintf "%d " ;
                  result ] ;
              send_log "rep" (String.length result) t0 ezfio_msg
          | Message.GetWalkers n_walks ->
            begin
              send_log "req" msg_size t0 "get_walkers";
              let result = 
                random_walkers n_walks
              in
              ZMQ.Socket.send_all rep_socket result;
              send_log "rep" walkers_size t0 "get_walkers"
            end     
          | Message.Register (w,pid) -> 
            begin
              match !status with
              | Status.Queued   
              | Status.Running  ->
                begin
                  String.concat [ "Register : " ; 
                    (Compute_node.to_string w) ; " " ; 
                    (Pid.to_string pid) ]
                  |> send_log "req" msg_size t0;
                  add_worker w pid;
                  if (!status = Status.Queued) then
                      change_status Status.Running ;
                  ZMQ.Socket.send rep_socket "OK";
                  send_log "rep" 2 t0 "Register : OK" 
                end
              | Status.Stopping 
              | Status.Stopped  ->
                  ZMQ.Socket.send rep_socket "Failed";
            end
          | Message.Unregister (w,pid) -> 
            begin
              String.concat [ "Unregister : " ;
                (Compute_node.to_string w) ; " " ; 
                    (Pid.to_string pid) ]
              |> send_log "req" msg_size t0;
              ZMQ.Socket.send rep_socket "OK";
              del_worker w pid;
              String.concat [ "Unregister : "; 
                (Hashtbl.length workers_hash) |> Int.to_string ;
                " remaining" ] 
              |> send_log "rep" 2 t0 ;
              let n_connect = 
                n_connected workers_hash t0
              in
              match (daemon,n_connect) with 
              | (false,0) -> change_status Status.Stopped
              | (true ,0) -> change_status Status.Queued 
              | _         -> ()
            end
          | Message.Test -> 
            begin
              ZMQ.Socket.send rep_socket "OK";
              send_log "rep" 2 t0 "Test"
            end
          | Message.Walkers (_, _, _)
          | Message.Property _
              -> failwith "Bad message"
        in handle msg
      in

      (** Handles messages coming into the PULL socket. *)
      let handle_pull status =
        let raw_msg =
          ZMQ.Socket.recv_all ~block:false pull_socket
        in
        let t0 = 
          Time.now ()
        in
        let msg =
          List.map ~f:String.strip raw_msg
          |> Message.create 
        and msg_size =
          List.fold ~init:0 ~f:(fun accu x -> accu + (String.length x)) raw_msg
        in
        let recv_log = 
          send_log "pull" msg_size t0
        in

        let handle = function
          | Message.Walkers (h,pid,w) ->
            begin
              if (status = Status.Running) then
                  touch_worker h pid ;
              let log_msg = 
                Printf.sprintf "Walkers from %s : %d / %d / %d"
                  (key h pid) (Array.length w) (!last_walker) walk_num_tot
              in
              recv_log log_msg ;
              for i=0 to ((Array.length w)-1)
              do
                Array.replace walkers_array (!last_walker) (fun _ -> Array.map 
                   ~f:Float.to_string w.(i));
                increment_last_walker ();
              done;
              let wall =
                Printf.sprintf "%f %f # %s %s %s %d"
                  (Time.Span.to_sec (Time.abs_diff (Time.now ()) wall0))
                  1. (Property.to_string Property.Wall)
                  hostname (Pid.to_string dataserver_pid) 1
                |> Block.of_string 
              in
              match wall with
              | Some wall -> 
                begin
                  Out_channel.output_string !block_channel (Block.to_string wall);
                  Out_channel.output_char   !block_channel '\n';
                end
              | _ -> ()
            end
          | Message.Property b ->
            begin
              if (status = Status.Running) then
                touch_worker b.Block.compute_node b.Block.pid ;
              Out_channel.output_string !block_channel (Block.to_string b);
              Out_channel.output_char   !block_channel '\n';
              recv_log (Block.to_string b)
            end
          | Message.Test
          | Message.GetWalkers _
          | Message.Ezfio _
          | Message.Register (_, _)
          | Message.Unregister (_, _)
            -> failwith "Bad message" 
        in handle msg
      in

      (* Main loop *)
      while (!status <> Status.Stopped)
      do
        let polling =
          ZMQ.Poll.poll ~timeout:1000 pollitem 
        in
        match polling.(1) with
        | Some ZMQ.Poll.In -> handle_pull !status
        | _ -> 
          begin
            match polling.(0) with
            | Some ZMQ.Poll.In -> handle_rep ()
            | _ -> 
            begin
              Out_channel.flush !block_channel ;
              let file_size = 
                (Unix.stat block_channel_filename_locked).Unix.st_size
                |> Float.of_int64
                |> Byte_units.create `Bytes 
              in
              if (file_size > !max_file_size) then
              begin
                compress_block_file ();
                max_file_size := Byte_units.scale file_size 1.2;
              end
            end
          end
      done;

      List.iter ~f:(fun socket ->
        ZMQ.Socket.set_linger_period socket 1000 ;
        ZMQ.Socket.close socket)
      [ rep_socket ; pull_socket ] 
    in
    Thread.create f
  in


  
  (** {2 Finalization} *)

  (** Cleans all the open files, sockets, etc.
      @param t0 is the initial time of the run, such that the wall time can be computed.
  *)
  let finalize ~t0 =
    print_string "Finalizing...";
    change_status Status.Stopped;
    compress_block_file ();
    send_log "status" 0 t0 "Done";
    close_debug_socket ();
    ZMQ.Context.terminate zmq_context;
    begin
      try
        Out_channel.close !block_channel;
        Unix.remove block_channel_filename_locked
      with
      | _ -> ()
    end;
    Qmcchem_result.display_summary ();
  in

  (** {3 Main function} *)

  let t0 = 
    Time.now ()
  in

  (* Handle signals *)
  let handler s = 
    Watchdog.kill ();
  in
  List.iter [
     Signal.term ;
     Signal.quit ;
     Signal.int  
    ]
    ~f:(fun x -> Signal.Expert.handle x handler)
  ;

  (* Run threads *)
  begin

    try
      (List.iter ~f:Thread.join 
        [ start_status_thread () ;
          start_log_thread ()   ;
          start_main_thread ()   ;
        ])
    with
    | err ->
      begin
        print_endline "Trapped error. Waiting 10 seconds...";
        change_status Status.Stopping;
        Time.Span.of_sec 10. |> Time.pause;
        finalize ~t0;
        raise err
      end
  end;
  finalize ~t0


