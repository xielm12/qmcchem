open Core.Std


let run ~t filename=

  Ezfio.set_file filename;

  if (not (Ezfio.has_simulation_http_server ())) then
     failwith "QMC=Chem is not running"
  ;

  let zmq_context =
    ZMQ.Context.create ()
  in

  Printf.printf "Debugging %s\n%!" filename;
  let socket =
    ZMQ.Socket.create zmq_context ZMQ.Socket.sub
  in

  let address = 
    match (Ezfio.get_simulation_http_server ()
        |> String.rsplit2 ~on:':' )
    with
    | Some (a,p) -> a^":"^( (Int.of_string p)+4 |> Int.to_string )
    | None -> failwith "Badly formed address"
  in
  ZMQ.Socket.connect socket address;
  ZMQ.Socket.subscribe socket "";

  if t then
      begin
        let re_split =
           Str.regexp " *: *"
        in
        let tot_size = 
          ref (Byte_units.create `Bytes 0.)
        in
        while true
        do
          let msg = 
            ZMQ.Socket.recv socket
          in
          let (socket, bytes)  =
            match Str.split re_split msg with
            |  socket :: bytes :: _ ->
                (socket, Byte_units.create `Bytes (Float.of_string bytes))
            | _ -> (print_endline msg ; ("", Byte_units.create `Bytes 0.))
          in
          tot_size := Byte_units.create `Bytes ((Byte_units.bytes !tot_size) +. (Byte_units.bytes bytes));
          Printf.printf "%s\n%!" (Byte_units.to_string !tot_size);
          Time.pause (Time.Span.of_float 1.)
        done
      end
  else
      begin
        while true
        do
          let msg = 
            ZMQ.Socket.recv socket
          in
          Printf.printf "%s\n%!" msg;
        done
      end



let spec =
  let open Command.Spec in
  empty
  +> flag "t" no_arg 
     ~doc:"Measure the throughput"
  +> anon ("filename" %: string)


let command = 
  Command.basic
  ~summary: "Debug ZeroMQ communications"
  ~readme:(fun () -> "Gets debug information from the ZMQ debug sockets.")
  spec
  (fun t filename () -> run t filename)



