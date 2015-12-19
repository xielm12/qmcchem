open Core.Std


let command =
    Command.group ~summary:"QMC=Chem command" [
          "debug" , Qmcchem_debug.command ;
          "edit"  , Qmcchem_edit.command ;
          "md5"   , Qmcchem_md5.command ;
          "result", Qmcchem_result.command ;
          "run"   , Qmcchem_run.command ;
          "stop"  , Qmcchem_stop.command ;
    ]

let () = 
  Command.run command
