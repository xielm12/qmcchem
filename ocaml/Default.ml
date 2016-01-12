open Core.Std;;


let simulation_do_nucl_fitcusp  = lazy(
  if (not (Ezfio.has_pseudo_do_pseudo ())) then
    not (Ezfio.get_pseudo_do_pseudo ())
  else
    true
)

let electrons_elec_walk_num        = lazy ( 30          )
let electrons_elec_walk_num_tot    = lazy ( 10000       )
let jastrow_jast_type              = lazy ( "None"      )
let simulation_block_time          = lazy ( 30          )
let simulation_ci_threshold        = lazy ( 1.e-8     )
let simulation_method              = lazy ( "VMC"       )
let simulation_sampling            = lazy ( "Langevin"  )
let simulation_stop_time           = lazy ( 3600        )
let simulation_time_step           = lazy ( 0.15        )
let simulation_dmc_projection_time = lazy ( 1.        )
                                 
let reset_defaults () =
  List.iter ~f:(fun x -> Sys.remove ( (Lazy.force Qputils.ezfio_filename) ^ x))
    [ "/electrons/elec_walk_num"       ;
      "/electrons/elec_walk_num_tot"   ;
      "/jastrow/jast_type"             ;
      "/simulation/block_time"         ;
      "/simulation/ci_threshold"       ;
      "/simulation/do_nucl_fitcusp"    ;
      "/simulation/method"             ;
      "/simulation/sampling"           ;
      "/simulation/stop_time"          ;
      "/simulation/time_step"          ]

