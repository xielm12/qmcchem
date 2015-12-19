open Core.Std

let split_re = 
  Str.regexp " +"


let split s =
  String.strip s
  |> Str.split split_re


let ezfio_filename = lazy (
  let f =
    !Ezfio.ezfio_filename
  in
  let full_path = 
    begin
      if f = "EZFIO_File" then
        begin
          if (Array.length Sys.argv = 1) then
             failwith "Error : EZFIO directory not specified on the command line\n";
          let ezfio_filename = Sys.argv.(1)
          in
          let () =
            match (Sys.is_directory ezfio_filename) with
            | `Yes -> Ezfio.set_file ezfio_filename ;
            | _ -> failwith ("Error : "^ezfio_filename^" not found")
          in ezfio_filename
        end
      else
        f
    end
  in
  let dir, result =
    Filename.realpath full_path
    |> Filename.split
  in
  Unix.chdir dir;
  result
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

