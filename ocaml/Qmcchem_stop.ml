open Core.Std


let run ezfio_filename = 
  Qputils.set_ezfio_filename ezfio_filename;
  Status.write Status.Stopping


let spec =
  let open Command.Spec in
  empty
  +> anon ("ezfio_file" %: string)

let command = 
    Command.basic
    ~summary: "Stop a running calculation"
    ~readme:(fun () ->
      "
Stop a running calculation
      ")
    spec
    (fun ezfio_file () ->  run ezfio_file )


