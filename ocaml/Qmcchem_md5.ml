open Core.Std

let run ?c ?d ~l ezfio_filename =

  Qputils.set_ezfio_filename ezfio_filename;

  let input_directory =
    Lazy.force Md5.input_directory
  in

  let handle_options () =

    let current_md5 = 
      Md5.hash ()
    in

    let filename_of_key key = 
      Filename.concat  input_directory  key
    in

    let key_is_valid key =
      let filename = 
        filename_of_key key
      in
      Sys.file_exists_exn filename
    in

    let () =
      match c with
      | None -> ()
      | Some new_md5 -> 
          if (key_is_valid new_md5) then
            Qmcchem_edit.run ~c:false ~input:(filename_of_key new_md5) ezfio_filename
          else
            failwith ("Error: " ^ new_md5 ^ " does not exist") 
    in

    let () = 
      match l with
      | false -> ()
      | true  ->
          Sys.ls_dir   input_directory
          |> List.iter ~f:(fun md5 -> 
              let filename =
                Filename.concat  input_directory  md5
              in
              let this =
                if (md5 = current_md5) then
                  "<-"
                else
                  ""
              in
              let date = 
                (Unix.stat filename).Unix.st_mtime
              in
              let date = 
                Unix.strftime (Unix.localtime date) "%a, %d %b %Y %T %z"
              in
              Printf.printf "%s : %s  %s\n" md5 date this)
    in

    let () =
      match d with
      | None -> ()
      | Some other_key ->
          if (key_is_valid other_key) then
            let command =
              String.concat ~sep:" "
               [ "diff" ; "-u" ; "-w" ;
                 (filename_of_key current_md5) ;
                 (filename_of_key other_key) ]
            in
            match (Unix.system command) with
            | _ -> ()
          else
            failwith ("Error: " ^ other_key ^ " does not exist") 
    in
    ()

  in

  match (c,d,l) with
  | (None,None,false) -> 
      Printf.printf "Current key :\n%s\n" (Md5.hash ())
  | _ -> handle_options ()


let spec =
  let open Command.Spec in
  empty
  +> flag "c" (optional string)
     ~doc:("<key> Change to input to <key>")
  +> flag "d" (optional string)
     ~doc:("<key> Show input differences with <key>")
  +> flag "l" no_arg
     ~doc:(" List all the saved MD5 keys.")
  +> anon ("ezfio_file" %: string)



let command =
    Command.basic
    ~summary: "Manipulate input MD5 keys"
    ~readme:(fun () ->
      "
Manipulate input MD5 keys
      ")
    spec
    (fun c d l ezfio_file () -> run ?c ?d ~l ezfio_file )




