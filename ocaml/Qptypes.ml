open Core.Std
let warning = print_string



module Positive_float : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= 0.) ; x )
  let to_string x = Float.to_string x
end


module Strictly_positive_float : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x > 0.) ; x )
  let to_string x = Float.to_string x
end


module Negative_float : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x <= 0.) ; x )
  let to_string x = Float.to_string x
end


module Strictly_negative_float : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x < 0.) ; x )
  let to_string x = Float.to_string x
end


module Positive_int : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x >= 0) ; x )
  let to_string x = Int.to_string x
end


module Strictly_positive_int : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; x )
  let to_string x = Int.to_string x
end


module Negative_int : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x <= 0) ; x )
  let to_string x = Int.to_string x
end


module Det_coef : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= -1.) ; 
  assert (x <=  1.) ; x )
  let to_string x = Float.to_string x
end


module Normalized_float : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x <= 1.) ; 
  assert (x >= 0.) ; x )
  let to_string x = Float.to_string x
end


module Strictly_negative_int : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x < 0) ; x )
  let to_string x = Int.to_string x
end


module Non_empty_string : sig
  type t with sexp
  val to_string : t -> string
  val of_string :  string -> t
  val to_string : t -> string
end = struct
  type t = string with sexp
  let to_string x = x
  let of_string  x = ( assert (x <> "") ; x )
  let to_string x = String.to_string x
end


module Det_number_max : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; 
  if (x > 100000000) then
    warning "More than 100 million determinants"; x )
  let to_string x = Int.to_string x
end


module MO_coef : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = (  x )
  let to_string x = Float.to_string x
end


module MO_occ : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= 0.); x )
  let to_string x = Float.to_string x
end


module AO_coef : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = (  x )
  let to_string x = Float.to_string x
end


module AO_expo : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= 0.) ; x )
  let to_string x = Float.to_string x
end


module AO_prim_number : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; x )
  let to_string x = Int.to_string x
end


module Threshold : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= 0.) ;
  assert (x <= 1.) ; x )
  let to_string x = Float.to_string x
end


module PT2_energy : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >=0.) ; x )
  let to_string x = Float.to_string x
end


module Elec_alpha_number : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; x )
  let to_string x = Int.to_string x
end


module Elec_beta_number : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x >= 0) ; x )
  let to_string x = Int.to_string x
end


module Elec_number : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; x )
  let to_string x = Int.to_string x
end


module MD5 : sig
  type t with sexp
  val to_string : t -> string
  val of_string :  string -> t
  val to_string : t -> string
end = struct
  type t = string with sexp
  let to_string x = x
  let of_string  x = ( assert ((String.length x) = 32); x )
  let to_string x = String.to_string x
end


module Rst_string : sig
  type t with sexp
  val to_string : t -> string
  val of_string :  string -> t
  val to_string : t -> string
end = struct
  type t = string with sexp
  let to_string x = x
  let of_string  x = (  x )
  let to_string x = String.to_string x
end


module Weight : sig
  type t with sexp
  val to_float : t -> float
  val of_float :  float -> t
  val to_string : t -> string
end = struct
  type t = float with sexp
  let to_float x = x
  let of_float  x = ( assert (x >= 0.) ; x )
  let to_string x = Float.to_string x
end


module Block_id : sig
  type t with sexp
  val to_int : t -> int
  val of_int :  int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_int x = x
  let of_int  x = ( assert (x > 0) ; x )
  let to_string x = Int.to_string x
end


module Compute_node : sig
  type t with sexp
  val to_string : t -> string
  val of_string :  string -> t
  val to_string : t -> string
end = struct
  type t = string with sexp
  let to_string x = x
  let of_string  x = ( assert (x <> "") ; x )
  let to_string x = String.to_string x
end



module MO_number : sig
  type t with sexp
  val to_int : t -> int
  val get_max : unit -> int
  val of_int : ?min:int -> ?max:int -> int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_string x = Int.to_string x
  let get_max () =
    if (Ezfio.has_mo_basis_mo_tot_num ()) then
      Ezfio.get_mo_basis_mo_tot_num ()
    else
      10000
  let get_min () =
      1
  let to_int x = x
  let of_int ?(min=get_min ()) ?(max=get_max ()) x = 
    begin
      assert (x >= min) ;
      if (x > 10000) then
        warning "More than 10000 MOs";
      begin
        match max with
        | 1 -> ()
        | i  -> assert ( x <= i )
      end ;
      x
    end
end

module AO_number : sig
  type t with sexp
  val to_int : t -> int
  val get_max : unit -> int
  val of_int : ?min:int -> ?max:int -> int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_string x = Int.to_string x
  let get_max () =
    if (Ezfio.has_ao_basis_ao_num ()) then
      Ezfio.get_ao_basis_ao_num ()
    else
      10000
  let get_min () =
      1
  let to_int x = x
  let of_int ?(min=get_min ()) ?(max=get_max ()) x = 
    begin
      assert (x >= min) ;
      if (x > 10000) then
        warning "More than 10000 AOs";
      begin
        match max with
        | 1 -> ()
        | i  -> assert ( x <= i )
      end ;
      x
    end
end

module Nucl_number : sig
  type t with sexp
  val to_int : t -> int
  val get_max : unit -> int
  val of_int : ?min:int -> ?max:int -> int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let to_string x = Int.to_string x
  let get_max () =
    if (Ezfio.has_nuclei_nucl_num ()) then
      Ezfio.get_nuclei_nucl_num ()
    else
      10000
  let get_min () =
      1
  let to_int x = x
  let of_int ?(min=get_min ()) ?(max=get_max ()) x = 
    begin
      assert (x >= min) ;
      if (x > 10000) then
        warning "More than 10000 nuclei";
      begin
        match max with
        | 1 -> ()
        | i  -> assert ( x <= i )
      end ;
      x
    end
end

let decode_ezfio_message msg =
match msg with 
 | "get_blocks_empty" -> Ezfio.read_string "blocks" "empty"
 | "get_mo_basis_mo_tot_num" -> Ezfio.read_string "mo_basis" "mo_tot_num"
 | "get_mo_basis_mo_coef" -> 
             Ezfio.read_string_array "mo_basis" "mo_coef"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_mo_basis_mo_classif" -> 
             Ezfio.read_string_array "mo_basis" "mo_classif"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_mo_basis_mo_energy" -> 
             Ezfio.read_string_array "mo_basis" "mo_energy"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_mo_basis_mo_occ" -> 
             Ezfio.read_string_array "mo_basis" "mo_occ"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_mo_basis_mo_symmetry" -> 
             Ezfio.read_string_array "mo_basis" "mo_symmetry"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_ao_pseudo_grid" -> 
             Ezfio.read_string_array "pseudo" "ao_pseudo_grid"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_do_pseudo" -> Ezfio.read_string "pseudo" "do_pseudo"
 | "get_pseudo_mo_pseudo_grid" -> 
             Ezfio.read_string_array "pseudo" "mo_pseudo_grid"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_dz_k" -> 
             Ezfio.read_string_array "pseudo" "pseudo_dz_k"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_dz_kl" -> 
             Ezfio.read_string_array "pseudo" "pseudo_dz_kl"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_grid_rmax" -> Ezfio.read_string "pseudo" "pseudo_grid_rmax"
 | "get_pseudo_pseudo_grid_size" -> Ezfio.read_string "pseudo" "pseudo_grid_size"
 | "get_pseudo_pseudo_klocmax" -> Ezfio.read_string "pseudo" "pseudo_klocmax"
 | "get_pseudo_pseudo_kmax" -> Ezfio.read_string "pseudo" "pseudo_kmax"
 | "get_pseudo_pseudo_lmax" -> Ezfio.read_string "pseudo" "pseudo_lmax"
 | "get_pseudo_pseudo_n_k" -> 
             Ezfio.read_string_array "pseudo" "pseudo_n_k"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_n_kl" -> 
             Ezfio.read_string_array "pseudo" "pseudo_n_kl"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_v_k" -> 
             Ezfio.read_string_array "pseudo" "pseudo_v_k"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_pseudo_pseudo_v_kl" -> 
             Ezfio.read_string_array "pseudo" "pseudo_v_kl"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ezfio_creation" -> Ezfio.read_string "ezfio" "creation"
 | "get_ezfio_user" -> Ezfio.read_string "ezfio" "user"
 | "get_ezfio_library" -> Ezfio.read_string "ezfio" "library"
 | "get_ezfio_last_library" -> Ezfio.read_string "ezfio" "last_library"
 | "get_simulation_do_run" -> Ezfio.read_string "simulation" "do_run"
 | "get_simulation_stop_time" -> Ezfio.read_string "simulation" "stop_time"
 | "get_simulation_equilibration" -> Ezfio.read_string "simulation" "equilibration"
 | "get_simulation_title" -> Ezfio.read_string "simulation" "title"
 | "get_simulation_http_server" -> Ezfio.read_string "simulation" "http_server"
 | "get_simulation_do_jast" -> Ezfio.read_string "simulation" "do_jast"
 | "get_simulation_do_nucl_fitcusp" -> Ezfio.read_string "simulation" "do_nucl_fitcusp"
 | "get_simulation_method" -> Ezfio.read_string "simulation" "method"
 | "get_simulation_block_time" -> Ezfio.read_string "simulation" "block_time"
 | "get_simulation_sampling" -> Ezfio.read_string "simulation" "sampling"
 | "get_simulation_save_data" -> Ezfio.read_string "simulation" "save_data"
 | "get_simulation_time_step" -> Ezfio.read_string "simulation" "time_step"
 | "get_simulation_print_level" -> Ezfio.read_string "simulation" "print_level"
 | "get_simulation_ci_threshold" -> Ezfio.read_string "simulation" "ci_threshold"
 | "get_simulation_md5_key" -> Ezfio.read_string "simulation" "md5_key"
 | "get_simulation_orig_time" -> Ezfio.read_string "simulation" "orig_time"
 | "get_simulation_e_ref" -> Ezfio.read_string "simulation" "e_ref"
 | "get_spindeterminants_n_det_alpha" -> Ezfio.read_string "spindeterminants" "n_det_alpha"
 | "get_spindeterminants_n_det_beta" -> Ezfio.read_string "spindeterminants" "n_det_beta"
 | "get_spindeterminants_n_det" -> Ezfio.read_string "spindeterminants" "n_det"
 | "get_spindeterminants_n_int" -> Ezfio.read_string "spindeterminants" "n_int"
 | "get_spindeterminants_bit_kind" -> Ezfio.read_string "spindeterminants" "bit_kind"
 | "get_spindeterminants_n_states" -> Ezfio.read_string "spindeterminants" "n_states"
 | "get_spindeterminants_psi_det_alpha" -> 
             Ezfio.read_string_array "spindeterminants" "psi_det_alpha"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_spindeterminants_psi_det_beta" -> 
             Ezfio.read_string_array "spindeterminants" "psi_det_beta"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_spindeterminants_psi_coef_matrix_rows" -> 
             Ezfio.read_string_array "spindeterminants" "psi_coef_matrix_rows"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_spindeterminants_psi_coef_matrix_columns" -> 
             Ezfio.read_string_array "spindeterminants" "psi_coef_matrix_columns"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_spindeterminants_psi_coef_matrix_values" -> 
             Ezfio.read_string_array "spindeterminants" "psi_coef_matrix_values"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ao_basis_ao_num" -> Ezfio.read_string "ao_basis" "ao_num"
 | "get_ao_basis_ao_prim_num" -> 
             Ezfio.read_string_array "ao_basis" "ao_prim_num"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ao_basis_ao_nucl" -> 
             Ezfio.read_string_array "ao_basis" "ao_nucl"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ao_basis_ao_power" -> 
             Ezfio.read_string_array "ao_basis" "ao_power"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ao_basis_ao_coef" -> 
             Ezfio.read_string_array "ao_basis" "ao_coef"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_ao_basis_ao_expo" -> 
             Ezfio.read_string_array "ao_basis" "ao_expo"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_electrons_elec_alpha_num" -> Ezfio.read_string "electrons" "elec_alpha_num"
 | "get_electrons_elec_beta_num" -> Ezfio.read_string "electrons" "elec_beta_num"
 | "get_electrons_elec_walk_num_tot" -> Ezfio.read_string "electrons" "elec_walk_num_tot"
 | "get_electrons_elec_walk_num" -> Ezfio.read_string "electrons" "elec_walk_num"
 | "get_electrons_elec_coord_pool" -> 
             Ezfio.read_string_array "electrons" "elec_coord_pool"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_electrons_elec_coord_pool_size" -> Ezfio.read_string "electrons" "elec_coord_pool_size"
 | "get_electrons_elec_fitcusp_radius" -> Ezfio.read_string "electrons" "elec_fitcusp_radius"
 | "get_jastrow_jast_type" -> Ezfio.read_string "jastrow" "jast_type"
 | "get_jastrow_jast_a_up_up" -> Ezfio.read_string "jastrow" "jast_a_up_up"
 | "get_jastrow_jast_a_up_dn" -> Ezfio.read_string "jastrow" "jast_a_up_dn"
 | "get_jastrow_jast_b_up_up" -> Ezfio.read_string "jastrow" "jast_b_up_up"
 | "get_jastrow_jast_b_up_dn" -> Ezfio.read_string "jastrow" "jast_b_up_dn"
 | "get_jastrow_jast_pen" -> 
             Ezfio.read_string_array "jastrow" "jast_pen"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_een_e_a" -> 
             Ezfio.read_string_array "jastrow" "jast_een_e_a"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_een_e_b" -> 
             Ezfio.read_string_array "jastrow" "jast_een_e_b"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_een_n" -> 
             Ezfio.read_string_array "jastrow" "jast_een_n"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_core_a1" -> 
             Ezfio.read_string_array "jastrow" "jast_core_a1"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_core_a2" -> 
             Ezfio.read_string_array "jastrow" "jast_core_a2"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_core_b1" -> 
             Ezfio.read_string_array "jastrow" "jast_core_b1"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_jastrow_jast_core_b2" -> 
             Ezfio.read_string_array "jastrow" "jast_core_b2"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_nuclei_nucl_num" -> Ezfio.read_string "nuclei" "nucl_num"
 | "get_nuclei_nucl_label" -> 
             Ezfio.read_string_array "nuclei" "nucl_label"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_nuclei_nucl_charge" -> 
             Ezfio.read_string_array "nuclei" "nucl_charge"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_nuclei_nucl_coord" -> 
             Ezfio.read_string_array "nuclei" "nucl_coord"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_nuclei_nucl_fitcusp_radius" -> 
             Ezfio.read_string_array "nuclei" "nucl_fitcusp_radius"
             |> Ezfio.flattened_ezfio 
             |> Array.to_list
             |> String.concat ~sep:" "
 | "get_properties_d_var_jast_a_up_dn" -> Ezfio.read_string "properties" "d_var_jast_a_up_dn"
 | "get_properties_d_var_jast_a_up_up" -> Ezfio.read_string "properties" "d_var_jast_a_up_up"
 | "get_properties_d_var_jast_b_up_dn" -> Ezfio.read_string "properties" "d_var_jast_b_up_dn"
 | "get_properties_d_var_jast_b_up_up" -> Ezfio.read_string "properties" "d_var_jast_b_up_up"
 | "get_properties_d_var_jast_core_a1" -> Ezfio.read_string "properties" "d_var_jast_core_a1"
 | "get_properties_d_var_jast_core_b1" -> Ezfio.read_string "properties" "d_var_jast_core_b1"
 | "get_properties_d_var_jast_een_e_a" -> Ezfio.read_string "properties" "d_var_jast_een_e_a"
 | "get_properties_d_var_jast_een_e_b" -> Ezfio.read_string "properties" "d_var_jast_een_e_b"
 | "get_properties_d_var_jast_een_n" -> Ezfio.read_string "properties" "d_var_jast_een_n"
 | "get_properties_d_var_jast_pen" -> Ezfio.read_string "properties" "d_var_jast_pen"
 | "get_properties_density1d" -> Ezfio.read_string "properties" "density1d"
 | "get_properties_dipole" -> Ezfio.read_string "properties" "dipole"
 | "get_properties_drift_mod" -> Ezfio.read_string "properties" "drift_mod"
 | "get_properties_e_kin" -> Ezfio.read_string "properties" "e_kin"
 | "get_properties_e_loc" -> Ezfio.read_string "properties" "e_loc"
 | "get_properties_e_loc_one" -> Ezfio.read_string "properties" "e_loc_one"
 | "get_properties_e_loc_per_electron" -> Ezfio.read_string "properties" "e_loc_per_electron"
 | "get_properties_e_loc_split_core" -> Ezfio.read_string "properties" "e_loc_split_core"
 | "get_properties_e_loc_two" -> Ezfio.read_string "properties" "e_loc_two"
 | "get_properties_e_nucl" -> Ezfio.read_string "properties" "e_nucl"
 | "get_properties_e_pot" -> Ezfio.read_string "properties" "e_pot"
 | "get_properties_e_pot_one" -> Ezfio.read_string "properties" "e_pot_one"
 | "get_properties_n_s_inverted" -> Ezfio.read_string "properties" "n_s_inverted"
 | "get_properties_n_s_updated" -> Ezfio.read_string "properties" "n_s_updated"
 | "get_properties_n_s_updates" -> Ezfio.read_string "properties" "n_s_updates"
 | "get_properties_voronoi_charges" -> Ezfio.read_string "properties" "voronoi_charges"
 | "get_properties_voronoi_charges_covariance" -> Ezfio.read_string "properties" "voronoi_charges_covariance"
 | "get_properties_voronoi_dipoles" -> Ezfio.read_string "properties" "voronoi_dipoles"
 | "get_properties_wf_extension" -> Ezfio.read_string "properties" "wf_extension"
 | "has_blocks_empty" -> if (Ezfio.has_blocks_empty ()) then "T" else "F"
 | "has_mo_basis_mo_tot_num" -> if (Ezfio.has_mo_basis_mo_tot_num ()) then "T" else "F"
 | "has_mo_basis_mo_coef" -> if (Ezfio.has_mo_basis_mo_coef ()) then "T" else "F"
 | "has_mo_basis_mo_classif" -> if (Ezfio.has_mo_basis_mo_classif ()) then "T" else "F"
 | "has_mo_basis_mo_energy" -> if (Ezfio.has_mo_basis_mo_energy ()) then "T" else "F"
 | "has_mo_basis_mo_occ" -> if (Ezfio.has_mo_basis_mo_occ ()) then "T" else "F"
 | "has_mo_basis_mo_symmetry" -> if (Ezfio.has_mo_basis_mo_symmetry ()) then "T" else "F"
 | "has_pseudo_ao_pseudo_grid" -> if (Ezfio.has_pseudo_ao_pseudo_grid ()) then "T" else "F"
 | "has_pseudo_do_pseudo" -> if (Ezfio.has_pseudo_do_pseudo ()) then "T" else "F"
 | "has_pseudo_mo_pseudo_grid" -> if (Ezfio.has_pseudo_mo_pseudo_grid ()) then "T" else "F"
 | "has_pseudo_pseudo_dz_k" -> if (Ezfio.has_pseudo_pseudo_dz_k ()) then "T" else "F"
 | "has_pseudo_pseudo_dz_kl" -> if (Ezfio.has_pseudo_pseudo_dz_kl ()) then "T" else "F"
 | "has_pseudo_pseudo_grid_rmax" -> if (Ezfio.has_pseudo_pseudo_grid_rmax ()) then "T" else "F"
 | "has_pseudo_pseudo_grid_size" -> if (Ezfio.has_pseudo_pseudo_grid_size ()) then "T" else "F"
 | "has_pseudo_pseudo_klocmax" -> if (Ezfio.has_pseudo_pseudo_klocmax ()) then "T" else "F"
 | "has_pseudo_pseudo_kmax" -> if (Ezfio.has_pseudo_pseudo_kmax ()) then "T" else "F"
 | "has_pseudo_pseudo_lmax" -> if (Ezfio.has_pseudo_pseudo_lmax ()) then "T" else "F"
 | "has_pseudo_pseudo_n_k" -> if (Ezfio.has_pseudo_pseudo_n_k ()) then "T" else "F"
 | "has_pseudo_pseudo_n_kl" -> if (Ezfio.has_pseudo_pseudo_n_kl ()) then "T" else "F"
 | "has_pseudo_pseudo_v_k" -> if (Ezfio.has_pseudo_pseudo_v_k ()) then "T" else "F"
 | "has_pseudo_pseudo_v_kl" -> if (Ezfio.has_pseudo_pseudo_v_kl ()) then "T" else "F"
 | "has_ezfio_creation" -> if (Ezfio.has_ezfio_creation ()) then "T" else "F"
 | "has_ezfio_user" -> if (Ezfio.has_ezfio_user ()) then "T" else "F"
 | "has_ezfio_library" -> if (Ezfio.has_ezfio_library ()) then "T" else "F"
 | "has_ezfio_last_library" -> if (Ezfio.has_ezfio_last_library ()) then "T" else "F"
 | "has_simulation_do_run" -> if (Ezfio.has_simulation_do_run ()) then "T" else "F"
 | "has_simulation_stop_time" -> if (Ezfio.has_simulation_stop_time ()) then "T" else "F"
 | "has_simulation_equilibration" -> if (Ezfio.has_simulation_equilibration ()) then "T" else "F"
 | "has_simulation_title" -> if (Ezfio.has_simulation_title ()) then "T" else "F"
 | "has_simulation_http_server" -> if (Ezfio.has_simulation_http_server ()) then "T" else "F"
 | "has_simulation_do_jast" -> if (Ezfio.has_simulation_do_jast ()) then "T" else "F"
 | "has_simulation_do_nucl_fitcusp" -> if (Ezfio.has_simulation_do_nucl_fitcusp ()) then "T" else "F"
 | "has_simulation_method" -> if (Ezfio.has_simulation_method ()) then "T" else "F"
 | "has_simulation_block_time" -> if (Ezfio.has_simulation_block_time ()) then "T" else "F"
 | "has_simulation_sampling" -> if (Ezfio.has_simulation_sampling ()) then "T" else "F"
 | "has_simulation_save_data" -> if (Ezfio.has_simulation_save_data ()) then "T" else "F"
 | "has_simulation_time_step" -> if (Ezfio.has_simulation_time_step ()) then "T" else "F"
 | "has_simulation_print_level" -> if (Ezfio.has_simulation_print_level ()) then "T" else "F"
 | "has_simulation_ci_threshold" -> if (Ezfio.has_simulation_ci_threshold ()) then "T" else "F"
 | "has_simulation_md5_key" -> if (Ezfio.has_simulation_md5_key ()) then "T" else "F"
 | "has_simulation_orig_time" -> if (Ezfio.has_simulation_orig_time ()) then "T" else "F"
 | "has_simulation_e_ref" -> if (Ezfio.has_simulation_e_ref ()) then "T" else "F"
 | "has_spindeterminants_n_det_alpha" -> if (Ezfio.has_spindeterminants_n_det_alpha ()) then "T" else "F"
 | "has_spindeterminants_n_det_beta" -> if (Ezfio.has_spindeterminants_n_det_beta ()) then "T" else "F"
 | "has_spindeterminants_n_det" -> if (Ezfio.has_spindeterminants_n_det ()) then "T" else "F"
 | "has_spindeterminants_n_int" -> if (Ezfio.has_spindeterminants_n_int ()) then "T" else "F"
 | "has_spindeterminants_bit_kind" -> if (Ezfio.has_spindeterminants_bit_kind ()) then "T" else "F"
 | "has_spindeterminants_n_states" -> if (Ezfio.has_spindeterminants_n_states ()) then "T" else "F"
 | "has_spindeterminants_psi_det_alpha" -> if (Ezfio.has_spindeterminants_psi_det_alpha ()) then "T" else "F"
 | "has_spindeterminants_psi_det_beta" -> if (Ezfio.has_spindeterminants_psi_det_beta ()) then "T" else "F"
 | "has_spindeterminants_psi_coef_matrix_rows" -> if (Ezfio.has_spindeterminants_psi_coef_matrix_rows ()) then "T" else "F"
 | "has_spindeterminants_psi_coef_matrix_columns" -> if (Ezfio.has_spindeterminants_psi_coef_matrix_columns ()) then "T" else "F"
 | "has_spindeterminants_psi_coef_matrix_values" -> if (Ezfio.has_spindeterminants_psi_coef_matrix_values ()) then "T" else "F"
 | "has_ao_basis_ao_num" -> if (Ezfio.has_ao_basis_ao_num ()) then "T" else "F"
 | "has_ao_basis_ao_prim_num" -> if (Ezfio.has_ao_basis_ao_prim_num ()) then "T" else "F"
 | "has_ao_basis_ao_nucl" -> if (Ezfio.has_ao_basis_ao_nucl ()) then "T" else "F"
 | "has_ao_basis_ao_power" -> if (Ezfio.has_ao_basis_ao_power ()) then "T" else "F"
 | "has_ao_basis_ao_coef" -> if (Ezfio.has_ao_basis_ao_coef ()) then "T" else "F"
 | "has_ao_basis_ao_expo" -> if (Ezfio.has_ao_basis_ao_expo ()) then "T" else "F"
 | "has_electrons_elec_alpha_num" -> if (Ezfio.has_electrons_elec_alpha_num ()) then "T" else "F"
 | "has_electrons_elec_beta_num" -> if (Ezfio.has_electrons_elec_beta_num ()) then "T" else "F"
 | "has_electrons_elec_walk_num_tot" -> if (Ezfio.has_electrons_elec_walk_num_tot ()) then "T" else "F"
 | "has_electrons_elec_walk_num" -> if (Ezfio.has_electrons_elec_walk_num ()) then "T" else "F"
 | "has_electrons_elec_coord_pool" -> if (Ezfio.has_electrons_elec_coord_pool ()) then "T" else "F"
 | "has_electrons_elec_coord_pool_size" -> if (Ezfio.has_electrons_elec_coord_pool_size ()) then "T" else "F"
 | "has_electrons_elec_fitcusp_radius" -> if (Ezfio.has_electrons_elec_fitcusp_radius ()) then "T" else "F"
 | "has_jastrow_jast_type" -> if (Ezfio.has_jastrow_jast_type ()) then "T" else "F"
 | "has_jastrow_jast_a_up_up" -> if (Ezfio.has_jastrow_jast_a_up_up ()) then "T" else "F"
 | "has_jastrow_jast_a_up_dn" -> if (Ezfio.has_jastrow_jast_a_up_dn ()) then "T" else "F"
 | "has_jastrow_jast_b_up_up" -> if (Ezfio.has_jastrow_jast_b_up_up ()) then "T" else "F"
 | "has_jastrow_jast_b_up_dn" -> if (Ezfio.has_jastrow_jast_b_up_dn ()) then "T" else "F"
 | "has_jastrow_jast_pen" -> if (Ezfio.has_jastrow_jast_pen ()) then "T" else "F"
 | "has_jastrow_jast_een_e_a" -> if (Ezfio.has_jastrow_jast_een_e_a ()) then "T" else "F"
 | "has_jastrow_jast_een_e_b" -> if (Ezfio.has_jastrow_jast_een_e_b ()) then "T" else "F"
 | "has_jastrow_jast_een_n" -> if (Ezfio.has_jastrow_jast_een_n ()) then "T" else "F"
 | "has_jastrow_jast_core_a1" -> if (Ezfio.has_jastrow_jast_core_a1 ()) then "T" else "F"
 | "has_jastrow_jast_core_a2" -> if (Ezfio.has_jastrow_jast_core_a2 ()) then "T" else "F"
 | "has_jastrow_jast_core_b1" -> if (Ezfio.has_jastrow_jast_core_b1 ()) then "T" else "F"
 | "has_jastrow_jast_core_b2" -> if (Ezfio.has_jastrow_jast_core_b2 ()) then "T" else "F"
 | "has_nuclei_nucl_num" -> if (Ezfio.has_nuclei_nucl_num ()) then "T" else "F"
 | "has_nuclei_nucl_label" -> if (Ezfio.has_nuclei_nucl_label ()) then "T" else "F"
 | "has_nuclei_nucl_charge" -> if (Ezfio.has_nuclei_nucl_charge ()) then "T" else "F"
 | "has_nuclei_nucl_coord" -> if (Ezfio.has_nuclei_nucl_coord ()) then "T" else "F"
 | "has_nuclei_nucl_fitcusp_radius" -> if (Ezfio.has_nuclei_nucl_fitcusp_radius ()) then "T" else "F"
 | "has_properties_d_var_jast_a_up_dn" -> if (Ezfio.has_properties_d_var_jast_a_up_dn ()) then "T" else "F"
 | "has_properties_d_var_jast_a_up_up" -> if (Ezfio.has_properties_d_var_jast_a_up_up ()) then "T" else "F"
 | "has_properties_d_var_jast_b_up_dn" -> if (Ezfio.has_properties_d_var_jast_b_up_dn ()) then "T" else "F"
 | "has_properties_d_var_jast_b_up_up" -> if (Ezfio.has_properties_d_var_jast_b_up_up ()) then "T" else "F"
 | "has_properties_d_var_jast_core_a1" -> if (Ezfio.has_properties_d_var_jast_core_a1 ()) then "T" else "F"
 | "has_properties_d_var_jast_core_b1" -> if (Ezfio.has_properties_d_var_jast_core_b1 ()) then "T" else "F"
 | "has_properties_d_var_jast_een_e_a" -> if (Ezfio.has_properties_d_var_jast_een_e_a ()) then "T" else "F"
 | "has_properties_d_var_jast_een_e_b" -> if (Ezfio.has_properties_d_var_jast_een_e_b ()) then "T" else "F"
 | "has_properties_d_var_jast_een_n" -> if (Ezfio.has_properties_d_var_jast_een_n ()) then "T" else "F"
 | "has_properties_d_var_jast_pen" -> if (Ezfio.has_properties_d_var_jast_pen ()) then "T" else "F"
 | "has_properties_density1d" -> if (Ezfio.has_properties_density1d ()) then "T" else "F"
 | "has_properties_dipole" -> if (Ezfio.has_properties_dipole ()) then "T" else "F"
 | "has_properties_drift_mod" -> if (Ezfio.has_properties_drift_mod ()) then "T" else "F"
 | "has_properties_e_kin" -> if (Ezfio.has_properties_e_kin ()) then "T" else "F"
 | "has_properties_e_loc" -> if (Ezfio.has_properties_e_loc ()) then "T" else "F"
 | "has_properties_e_loc_one" -> if (Ezfio.has_properties_e_loc_one ()) then "T" else "F"
 | "has_properties_e_loc_per_electron" -> if (Ezfio.has_properties_e_loc_per_electron ()) then "T" else "F"
 | "has_properties_e_loc_split_core" -> if (Ezfio.has_properties_e_loc_split_core ()) then "T" else "F"
 | "has_properties_e_loc_two" -> if (Ezfio.has_properties_e_loc_two ()) then "T" else "F"
 | "has_properties_e_nucl" -> if (Ezfio.has_properties_e_nucl ()) then "T" else "F"
 | "has_properties_e_pot" -> if (Ezfio.has_properties_e_pot ()) then "T" else "F"
 | "has_properties_e_pot_one" -> if (Ezfio.has_properties_e_pot_one ()) then "T" else "F"
 | "has_properties_n_s_inverted" -> if (Ezfio.has_properties_n_s_inverted ()) then "T" else "F"
 | "has_properties_n_s_updated" -> if (Ezfio.has_properties_n_s_updated ()) then "T" else "F"
 | "has_properties_n_s_updates" -> if (Ezfio.has_properties_n_s_updates ()) then "T" else "F"
 | "has_properties_voronoi_charges" -> if (Ezfio.has_properties_voronoi_charges ()) then "T" else "F"
 | "has_properties_voronoi_charges_covariance" -> if (Ezfio.has_properties_voronoi_charges_covariance ()) then "T" else "F"
 | "has_properties_voronoi_dipoles" -> if (Ezfio.has_properties_voronoi_dipoles ()) then "T" else "F"
 | "has_properties_wf_extension" -> if (Ezfio.has_properties_wf_extension ()) then "T" else "F"
 | x -> failwith (x^" : Unknown EZFIO function")
;;

