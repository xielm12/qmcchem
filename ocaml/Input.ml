open Core.Std
open Qptypes
open Qputils



module Pseudo: sig

  type t = bool
  val doc   : string
  val read  : unit -> t
  val to_bool : t -> bool
  val of_bool : bool -> t
  val to_int  : t -> int
  val of_int  : int -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = bool

  let doc = "Compute pseudo-potentials"

  let of_bool x = x 

  let to_bool x = x 

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_pseudo_do_pseudo ())) then
      Ezfio.set_pseudo_do_pseudo false;
    Ezfio.get_pseudo_do_pseudo ()
    |> of_bool


  let to_string t =
    to_bool t
    |> Bool.to_string


  let of_string t =
    try
      String.lowercase t
      |> Bool.of_string 
      |> of_bool
    with
    | Invalid_argument msg -> failwith msg


  let to_int t =
    let t = 
      to_bool t
    in
    if t then 1
    else 0


  let of_int = function
    | 0 -> false
    | 1 -> true
    | _ -> failwith "Expected 0 or 1"


end

module Fitcusp : sig

  type t = bool
  val doc   : string
  val read  : unit -> t
  val write : t -> unit
  val to_bool : t -> bool
  val of_bool : bool -> t
  val to_int  : t -> int
  val of_int  : int -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = bool

  let doc = "Correct wave function to verify electron-nucleus cusp condition"

  let of_bool x = x 

  let to_bool x = x 

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_do_nucl_fitcusp ())) then
      Lazy.force Default.simulation_do_nucl_fitcusp
      |> Ezfio.set_simulation_do_nucl_fitcusp ;
    Ezfio.get_simulation_do_nucl_fitcusp ()
    |> of_bool


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    let () =
      match (Pseudo.read () |> Pseudo.to_bool, to_bool t) with
      | (true, true) -> failwith "Pseudopotentials and Fitcusp are incompatible"
      | _ -> ()
    in
    to_bool t
    |> Ezfio.set_simulation_do_nucl_fitcusp


  let to_string t =
    to_bool t
    |> Bool.to_string


  let of_string t =
    try
      String.lowercase t
      |> Bool.of_string 
      |> of_bool
    with
    | Invalid_argument msg -> failwith msg


  let to_int t =
    let t = 
      to_bool t
    in
    if t then 1
    else 0


  let of_int = function
    | 0 -> false
    | 1 -> true
    | _ -> failwith "Expected 0 or 1"


end

module Block_time : sig

  type t = int
  val doc   : string
  val read  : unit -> t
  val write : t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val of_string : string -> t
  val to_float  : t -> float
  val of_float  : float-> t

end = struct

  type t = int

  let doc = "Time (seconds) of a block"

  let of_int x =
    if (x < 1) then
      failwith "Block time should be >=1";
    if (x > 36000) then
      failwith "Block time is too large (<= 36000)";
    x 


  let to_int x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_block_time ())) then
      Lazy.force Default.simulation_block_time
      |> Ezfio.set_simulation_block_time ;
    Ezfio.get_simulation_block_time ()
    |> of_int


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_int t
    |> Ezfio.set_simulation_block_time 


  let to_string t =
    to_int t
    |> Int.to_string


  let of_string t =
    Int.of_string t
    |> of_int


  let to_float  t =
    to_int t
    |> Float.of_int


  let of_float t =
    Int.of_float t
    |> of_int


end

module Walk_num : sig

  type t = int
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = int
  let doc = "Number of walkers per CPU core"

  let of_int x =
    if (x < 1) then
      failwith "Number of walkers should be >=1";
    if (x > 100_000) then
      failwith "Number of walkers is too large (<= 100_000)";
    x 


  let to_int x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_electrons_elec_walk_num () )) then
      Lazy.force Default.electrons_elec_walk_num
      |> Ezfio.set_electrons_elec_walk_num ;
    Ezfio.get_electrons_elec_walk_num ()
    |> of_int


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_int t
    |> Ezfio.set_electrons_elec_walk_num


  let to_string t =
    to_int t
    |> Int.to_string


  let of_string t =
    Int.of_string t
    |> of_int


end

module Walk_num_tot : sig

  type t = int
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = int
  let doc = "Total number of stored walkers for restart"

  let of_int x =
    if (x < 2) then
      failwith "Total number of stored walkers should be > 1";
    if (x > 100_000_000) then
      failwith "Number of walkers to store too large (<= 100.10^6)";
    x 


  let to_int x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_electrons_elec_walk_num_tot () )) then
      Lazy.force Default.electrons_elec_walk_num_tot
      |> Ezfio.set_electrons_elec_walk_num_tot ;
    Ezfio.get_electrons_elec_walk_num_tot ()
    |> of_int


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_int t
    |> Ezfio.set_electrons_elec_walk_num_tot


  let to_string t =
    to_int t
    |> Int.to_string


  let of_string t =
    Int.of_string t
    |> of_int


end


module Stop_time : sig

  type t = int
  val read  : unit -> t
  val doc : string
  val write : t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_float : t -> float 
  val of_float : float -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = int

  let doc = "Requested simulation time (seconds)"

  let of_int x =
    if (x < 1) then
      failwith "Simulation time too short (>=1 s)";
    x 


  let to_int x = x 

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_stop_time ())) then
      Lazy.force Default.simulation_stop_time
      |> Ezfio.set_simulation_stop_time ;
    Ezfio.get_simulation_stop_time ()
    |> of_int


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_int t
    |> Ezfio.set_simulation_stop_time 


  let to_string t =
    to_int t
    |> Int.to_string


  let of_string t =
    Int.of_string t
    |> of_int


  let to_float  t =
    to_int t
    |> Float.of_int


  let of_float t =
    Int.of_float t
    |> of_int

end



module Method : sig

  type t = VMC | DMC
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = VMC | DMC

  let doc = "QMC Method : [ VMC | DMC ]"

  let of_string = function
  | "VMC" | "vmc" -> VMC
  | "DMC" | "dmc" -> DMC
  | x -> failwith ("Method should be [ VMC | DMC ], not "^x^".")


  let to_string = function
  | VMC  -> "VMC"
  | DMC  -> "DMC"


  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_method ())) then
      Lazy.force Default.simulation_method
      |> Ezfio.set_simulation_method ;
    Ezfio.get_simulation_method ()
    |> of_string


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_string t
    |> Ezfio.set_simulation_method


end



module Sampling : sig

  type t = Brownian | Langevin
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = Brownian | Langevin

  let doc = "Sampling algorithm : [ Langevin | Brownian ]"

  let of_string s = 
    match String.capitalize (String.strip s) with
    | "Langevin" -> Langevin
    | "Brownian" -> Brownian
    | x -> failwith ("Sampling should be [ Brownian | Langevin ], not "^x^".")


  let to_string = function
  | Langevin -> "Langevin"
  | Brownian -> "Brownian"


  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_sampling ())) then
      Lazy.force Default.simulation_sampling
      |> Ezfio.set_simulation_sampling ;
    Ezfio.get_simulation_sampling ()
    |> of_string


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_string t
    |> Ezfio.set_simulation_sampling


end



module Ref_energy : sig

  type t = float
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_float  : t -> float 
  val of_float  : float -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = float
  let doc = "Fixed reference energy to normalize DMC weights (au)"

  let of_float x = 
    if (x > 0.) then
      failwith "Reference energy should not be positive.";
    if (x <= -1_000_000.) then
      failwith "Reference energy is too low.";
    x


  let to_float x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_e_ref ())) then
      to_float 0.
      |> Ezfio.set_simulation_e_ref;
    Ezfio.get_simulation_e_ref ()
    |> of_float


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_float t
    |> Ezfio.set_simulation_e_ref


  let of_string x =
     Float.of_string x
     |> of_float


  let to_string x =
    to_float x 
    |> Float.to_string 


end


module CI_threshold : sig

  type t = float
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_float  : t -> float 
  val of_float  : float -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = float
  let doc = "Truncation t of the wave function : Remove determinants with a
contribution to the norm less than t (au)"

  let of_float x = 
    if (x >= 1.) then
      failwith "Truncation of the wave function should be < 1.";
    if (x < 0.) then
      failwith "Truncation of the wave function should be positive.";
    x


  let to_float x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_ci_threshold ())) then
      Lazy.force Default.simulation_ci_threshold 
      |> Ezfio.set_simulation_ci_threshold ;
    Ezfio.get_simulation_ci_threshold ()
    |> of_float


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_float t
    |> Ezfio.set_simulation_ci_threshold


  let of_string x =
    Float.of_string x
    |> of_float


  let to_string x =
    to_float x
    |> Float.to_string 
  
end

module DMC_projection_time : sig

  type t = float
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_float  : t -> float 
  val of_float  : float -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = float
  let doc = "DMC projection time (au)"

  let of_float x = 
    if (x >= 100.) then
      failwith "DMC Projection time should be < 100.";
    if (x <= 0.) then
      failwith "DMC Projection time should be positive.";
    x


  let to_float x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_dmc_projection_time())) then
      Lazy.force Default.simulation_dmc_projection_time
      |> Ezfio.set_simulation_dmc_projection_time ;
    Ezfio.get_simulation_dmc_projection_time ()
    |> of_float


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_float t
    |> Ezfio.set_simulation_dmc_projection_time


  let of_string x =
    Float.of_string x
    |> of_float


  let to_string x =
    to_float x
    |> Float.to_string 

end

module Time_step : sig

  type t = float
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_float  : t -> float 
  val of_float  : float -> t
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = float
  let doc = "Simulation time step (au)"

  let of_float x = 
    if (x >= 10.) then
      failwith "Time step should be < 10.";
    if (x <= 0.) then
      failwith "Time step should be positive.";
    x


  let to_float x = x

  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_simulation_time_step ())) then
      Lazy.force Default.simulation_time_step
      |> Ezfio.set_simulation_time_step ;
    Ezfio.get_simulation_time_step ()
    |> of_float


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    to_float t
    |> Ezfio.set_simulation_time_step


  let of_string x =
    Float.of_string x
    |> of_float


  let to_string x =
    to_float x
    |> Float.to_string 

end

module Jastrow_type : sig

  type t = None | Core | Simple
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = None | Core | Simple
  let doc = "Type of Jastrow factor [ None | Core | Simple ]"

  let of_string s = 
    match String.capitalize (String.strip s) with
    | "Core" -> Core
    | "Simple" -> Simple
    | "None" -> None
    | _ -> failwith "Jastrow type should be [ None | Core | Simple ]"


  let to_string = function
  | Core -> "Core"
  | Simple -> "Simple"
  | None -> "None"


  let read () = 
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    if (not (Ezfio.has_jastrow_jast_type ())) then
      Lazy.force Default.jastrow_jast_type
      |> Ezfio.set_jastrow_jast_type ;
    Ezfio.get_jastrow_jast_type ();
    |> of_string


  let write t =
    let _ =
      Lazy.force Qputils.ezfio_filename
    in
    let () =
      match (Pseudo.read () |> Pseudo.to_bool, t) with
      | (false, _) 
      | (true , None) -> ()
      | _ -> failwith "Jastrow and Pseudopotentials are incompatible for now"
    in

    to_string t
    |> Ezfio.set_jastrow_jast_type


end

module Properties: sig

  type t = (Property.t * bool) list
  val doc : string
  val read  : unit -> t
  val write : t -> unit
  val to_string : t -> string
  val of_string : string -> t

end = struct

  type t = (Property.t * bool) list

  let doc = 
    "Properties to sample. (X) is true and ( ) is false"


  let read () =
    List.map Property.all ~f:(fun x -> (x, Property.calc x))


  let write l =
    List.iter l ~f:(fun (x,b) -> Property.set_calc x b)


  let to_string l =
    List.map l ~f:(fun (x,b) -> 
      let ch =
         if b then "X" else " "
      in
      Printf.sprintf "(%s) %s" ch (Property.to_string x))
    |> String.concat ~sep:"\n"


  let of_string s =
    String.split s ~on:'\n'
    |> List.map ~f:(fun x ->
       let (calc,prop) = 
         String.strip x
         |> String.rsplit2_exn ~on:' '
       in
       let prop = 
         String.strip prop 
         |> Property.of_string
       and calc =
         match calc with
         | "(X)" -> true
         | "( )" -> false
         | _ -> failwith " (X) or ( ) expected"
       in
       (prop, calc)
    )

end

(** Check if everything is correct in the input file. *)
let validate () =

  let _ = 
    Lazy.force Qputils.ezfio_filename
  in

  (* Check if walkers are present *)
  if (not (Ezfio.has_electrons_elec_coord_pool ())) then
    Printf.printf "Warning: No initial walkers\n";

  let meth = 
     Method.read ()
  and sampling = 
     Sampling.read ()
  and ts =
     Time_step.read ()
  and jast_type = 
    Jastrow_type.read ()
  and do_fitcusp =
    Fitcusp.read ()
  and do_pseudo =
    Pseudo.read ()
  in

  (* Check sampling and time steps *)
  let () =
    match (sampling, meth, Pseudo.to_bool do_pseudo) with
    | (Sampling.Brownian, Method.DMC, true) ->
      if ( (Time_step.to_float ts) >= 0.5 ) then
          warn "Time step seems large for DMC.";
    | (Sampling.Brownian, Method.DMC, false) ->
      if ( (Time_step.to_float ts) >= 0.01 ) then
          warn "Time step seems large for DMC.";
    | (Sampling.Brownian, Method.VMC, _) -> 
      if ( (Time_step.to_float ts) >= 10. ) then
          warn "Time step seems large for VMC.";
    | (Sampling.Langevin, Method.VMC, _) -> 
      if ( (Time_step.to_float ts) <= 0.01 ) then
          warn "Time step seems small for Langevin sampling."
    | (Sampling.Langevin, Method.DMC, _) ->
        failwith "Lanvegin sampling is incompatible with DMC"
  in


  (* Check E_ref is not zero *)
  let () =
    match (meth, Ref_energy.(read () |> to_float) ) with
    | (Method.DMC,0.) -> failwith "E_ref should not be zero in DMC"
    | _          -> ()
  in

  (* Set block and total time*)
  let () =
    if ( (Block_time.read ()) > Stop_time.read ()) then
       failwith "Block time is longer than total time"
  in

  (* Check if E_loc if computed *)
  let () =
    match (meth, Property.(calc E_loc)) with
    | (Method.DMC, false) -> failwith "E_loc should be sampled in DMC"
    | (Method.VMC, false) -> warn "Sampling of E_loc is not activated in input"
    | _ -> ()
  in

  (* Pseudo and Jastrow are incompatible *)
  let () = 
    match (Pseudo.to_bool do_pseudo, jast_type) with
    | (true, Jastrow_type.Core  ) 
    | (true, Jastrow_type.Simple) -> failwith "Jastrow and Pseudopotentials are incompatible"
    | _ -> ()
  in

  (* Fitcusp is not recommended with pseudo *)
  let () =
    match (Pseudo.to_bool do_pseudo, Fitcusp.to_bool do_fitcusp) with
    | (true, true) -> warn "Fitcusp is incompatible with Pseudopotentials"
    | _ -> ()
  in

  (* Other Checks *)
  let () =
    let _ =
      Walk_num.read ()
    and _ =
      Walk_num_tot.read ()
    and _ =
      CI_threshold.read ()
    in ()
  in
  ()



