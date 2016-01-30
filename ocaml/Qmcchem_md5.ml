open Core.Std

let run ?c ?d ~l ~update ezfio_filename =

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

    if (update) then
       begin
          Printf.printf "Updating\n%!" ;
          let update_one old_key =
            Qmcchem_edit.run ~c:false ~input:(filename_of_key old_key) ezfio_filename;
            Md5.reset_hash ();
            let new_key =
              Md5.hash ()
            in

            if (old_key <> new_key) then
              begin
                let new_name = 
                  String.concat ~sep:"/" [ ezfio_filename; "blocks"; new_key ]
                and old_name = 
                  String.concat ~sep:"/" [ ezfio_filename; "blocks"; old_key ]
                in
                Printf.printf "Renaming %s -> %s\n" old_name new_name;
                try Sys.rename old_name new_name with
                | Sys_error _ -> ();

                let old_name = 
                  String.concat ~sep:"/" [ ezfio_filename; "input"; old_key ]
                in
                Printf.printf "Removing %s\n%!" old_name;
                try Sys.remove old_name with
                | Sys_error _ -> ();
            end
          in
          let l = 
            Sys.ls_dir input_directory
          in
          List.iter l ~f:(fun x -> update_one x) ;
          Printf.printf "Done\n%!" ;
       end
    ;

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

  match (c,d,l,update) with
  | (None,None,false,false) -> 
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
  +> flag "update" no_arg
     ~doc:(" Update to the latest MD5 format.")
  +> anon ("ezfio_file" %: string)



let command =
    Command.basic
    ~summary: "Manipulate input MD5 keys"
    ~readme:(fun () ->
      "
Manipulate input MD5 keys
      ")
    spec
    (fun c d l update ezfio_file () -> run ?c ?d ~l ~update ezfio_file )




