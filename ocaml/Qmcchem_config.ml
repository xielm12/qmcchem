open Core.Std;;


(** QMC=Chem installation directory *)
let root = lazy (
  match ( Sys.getenv "QMCCHEM_PATH" ) with
  | Some x -> x
  | None   -> failwith "QMCCHEM_PATH environment variable not set"
)


(* PATH environment variable as a list of strings *)
let path =  lazy (
  let p = 
    match Sys.getenv "PATH" with
    | None -> failwith "PATH environment variable is not set"
    | Some p -> p
  in 
  String.split ~on:':' p
)



(* Full path of a binary taken from the PATH *)
let full_path exe =
  let rec in_path_rec = function
  | [] -> None
  | head :: tail ->
    begin
      let fp = 
        Filename.concat head exe 
      in
      match (Sys.is_file fp) with
      | `Yes -> Some fp
      | _    -> in_path_rec tail
    end
  in
  Lazy.force path
  |> in_path_rec



(* True if an executable is in the PATH *)
let in_path x =
   match (full_path x) with
   | Some _ -> true
   | None   -> false


let has_parallel   = lazy( in_path "parallel" )
let has_mpirun     = lazy( in_path "mpirun"   )
let has_srun       = lazy( in_path "parallel" )
let has_qmc        = lazy( in_path "qmc"      )
let has_qmc_mic    = lazy( in_path "qmc_mic"  )


let mpirun = lazy (
    match Sys.getenv "QMCCHEM_MPIRUN" with
    | None -> "mpirun"
    | Some p -> p
)

let qmcchem = lazy(
  Filename.concat (Lazy.force root) "bin/qmcchem"
)
and qmc = lazy(
  Filename.concat (Lazy.force root) "bin/qmc"
)
and qmcchem_info = lazy(
  Filename.concat (Lazy.force root) "bin/qmcchem_info"
)
and qmc_mic = lazy(
  Filename.concat (Lazy.force root) "bin/qmc_mic"
)
and qmc_create_walkers = lazy(
  Filename.concat (Lazy.force root) "bin/qmc_create_walkers"
)

let dev_shm = "/dev/shm/"

(** Name of the host on which the data server runs *)
let hostname = lazy (
  try
    Unix.gethostname ()
  with
  | _ -> "localhost"
)


let ip_address = lazy (
  match Sys.getenv "QMCCHEM_NIC" with
  | None -> 
      begin
        try
          Lazy.force hostname
          |> Unix.Inet_addr.of_string_or_getbyname
          |> Unix.Inet_addr.to_string
        with
        | Unix.Unix_error _ ->
            failwith "Unable to find IP address from host name."
      end
  | Some interface ->
      begin
        try
          ok_exn Linux_ext.get_ipv4_address_for_interface interface
        with
        | Unix.Unix_error _ ->
            Lazy.force hostname
            |> Unix.Inet_addr.of_string_or_getbyname
            |> Unix.Inet_addr.to_string
      end
)



