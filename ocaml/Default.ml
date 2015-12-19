open Core.Std;;

let simulation_do_nucl_fitcusp  = 
  if (not (Ezfio.has_simulation_do_nucl_fitcusp ())) then
    begin
      if (not (Ezfio.has_pseudo_do_pseudo ())) then
        true
      else
        not (Ezfio.get_pseudo_do_pseudo ())
    end
  else
    Ezfio.get_simulation_do_nucl_fitcusp ()


let electrons_elec_walk_num     =  30          
let electrons_elec_walk_num_tot =  10000       
let jastrow_jast_type           =  "None"      
let simulation_block_time       =  30          
let simulation_ci_threshold     =  1.e-8       
let simulation_method           =  "VMC"       
let simulation_sampling         =  "Langevin"  
let simulation_stop_time        =  3600        
let simulation_time_step        =  0.15        
                                 
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

