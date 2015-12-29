open Core.Std


let run ezfio_filename = 
  let qmcchem_info =
      Lazy.force Qmcchem_config.qmcchem_info
  in
  let prog, args = 
      qmcchem_info, 
    [ qmcchem_info ; ezfio_filename ]
  in
  ignore @@
    Unix.exec ~prog ~args () 


let spec =
  let open Command.Spec in
  empty
  +> anon ("ezfio_file" %: string)

let command = 
    Command.basic
    ~summary: "Display info on an EZFIO database"
    ~readme:(fun () ->
      "
Display info on an EZFIO database
      ")
    spec
    (fun ezfio_file () ->  run ezfio_file )


