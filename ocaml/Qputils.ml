open Core.Std

let split_re = 
  Str.regexp " +"


let split s =
  String.strip s
  |> Str.split split_re


let set_ezfio_filename ezfio_filename =
  let () = 
    if (not (Sys.file_exists_exn ezfio_filename)) then
      failwith (ezfio_filename^" does not exist")
  in
  let () =
    match (Sys.is_directory ezfio_filename) with
    | `Yes -> Ezfio.set_file ezfio_filename ;
    | _ -> failwith ("Error : "^ezfio_filename^" is not a directory")
  in 
  let dir, result =
    Filename.realpath ezfio_filename
    |> Filename.split
  in
  Unix.chdir dir ;
  Ezfio.set_file result
  

let ezfio_filename = lazy (
  let f =
    !Ezfio.ezfio_filename
  in
  let full_path = 
    match f with
    | "EZFIO_File" ->
        begin
          if (Array.length Sys.argv = 1) then
             failwith "Error : EZFIO directory not specified on the command line\n";
          Sys.argv.(1)
        end
    | f -> f
  in
  set_ezfio_filename full_path;
  !Ezfio.ezfio_filename
)


let elec_num = lazy (
  Ezfio.set_file (Lazy.force ezfio_filename);
  Ezfio.get_electrons_elec_alpha_num () +
  Ezfio.get_electrons_elec_beta_num  ()
)


let walk_num = lazy (
  Ezfio.set_file (Lazy.force ezfio_filename);
  Ezfio.get_electrons_elec_walk_num ()
)


let warn msg =
  Printf.printf "Warning : %s\n%!" msg

let () =
  Random.self_init ()

