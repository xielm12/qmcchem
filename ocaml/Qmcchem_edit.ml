open Core.Std

let file_header filename = Printf.sprintf
"
+----------------------------------------------------------------+
|                            QMC=Chem                            |
+----------------------------------------------------------------+

Editing file `%s`

" filename

let make_header s =
  let l = String.length s in
  "\n\n"^s^"\n"^(String.init l ~f:(fun _ -> '='))^"\n\n"


type field = 
 | Block_time
 | Walk_num
 | Walk_num_tot
 | Stop_time
 | Fitcusp
 | Method
 | Sampling
 | Ref_energy
 | CI_threshold
 | Time_step
 | Jastrow_type
 | Properties


let get field = 
  let option_to_string read to_string doc = 
    let value =
      read () |> to_string
    in
    Printf.sprintf "%s ::\n\n    %s\n\n" doc value
  in
  let option_to_string_prop read to_string doc = 
    let value =
      read () |> to_string
    in
    Printf.sprintf "%s :\n\n%s\n\n" doc value
  in
  let open Input in
  match field with
 | Block_time   ->
   option_to_string Block_time.read   Block_time.to_string   Block_time.doc  
 | Walk_num     ->
   option_to_string Walk_num.read     Walk_num.to_string     Walk_num.doc  
 | Walk_num_tot ->
   option_to_string Walk_num_tot.read Walk_num_tot.to_string Walk_num_tot.doc  
 | Stop_time    ->
   option_to_string Stop_time.read    Stop_time.to_string    Stop_time.doc  
 | Fitcusp      ->
   option_to_string Fitcusp.read      Fitcusp.to_string      Fitcusp.doc
 | Method       ->
   option_to_string Method.read       Method.to_string       Method.doc  
 | Sampling     ->
   option_to_string Sampling.read     Sampling.to_string     Sampling.doc  
 | Ref_energy   ->
   option_to_string Ref_energy.read   Ref_energy.to_string   Ref_energy.doc  
 | CI_threshold ->                    
   option_to_string CI_threshold.read   CI_threshold.to_string   CI_threshold.doc  
 | Time_step    ->                    
   option_to_string Time_step.read    Time_step.to_string    Time_step.doc  
 | Jastrow_type ->                    
   option_to_string Jastrow_type.read Jastrow_type.to_string Jastrow_type.doc  
 | Properties   ->                    
   option_to_string_prop Properties.read Properties.to_string Properties.doc  

                                      

let create_temp_file ?temp_filename ezfio_filename fields =
  let filename =
    match temp_filename with
    | None -> Filename.temp_file "qmcchem_edit_" ".rst"
    | Some name -> name
  in
  Out_channel.with_file filename ~f:(fun out_channel ->
      (file_header ezfio_filename) :: (List.map ~f:get fields)
      |> String.concat ~sep:"\n"
      |> Out_channel.output_string out_channel
    )
  ; filename


(** Write the input file corresponding to the MD5 key *)
let write_input_in_ezfio  ezfio_filename  fields =
  let dirname = 
    Lazy.force  Md5.input_directory
  in
  let temp_filename = 
    Md5.hash ()
    |> Filename.concat  dirname
  in
  let input_filename =
    create_temp_file  ~temp_filename  ezfio_filename  fields
  in
  assert (Sys.file_exists_exn  input_filename)


(** Run the edit command *)
let run ~c ?f ?t ?l ?m ?e ?s ?ts ?w ?wt ?n ?j ?input ezfio_filename =

  let interactive = ref (
    if c then
      false
    else
      true
  )
  in

  (* Open EZFIO *)
  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exist");

  Ezfio.set_file ezfio_filename;

  let handle_option (type_conv, write) x =
    let () =
      match x with
      | Some x -> 
        begin
          type_conv x |> write;
          interactive := false;
        end
      | None   -> ()
    in ();
  in

  handle_option   Input.Ref_energy.(of_float , write) e;
  handle_option Input.Jastrow_type.(of_string, write) j;
  handle_option   Input.Block_time.(of_int   , write) l;
  handle_option       Input.Method.(of_string, write) m;
  handle_option    Input.Stop_time.(of_int   , write) t;
  handle_option     Input.Sampling.(of_string, write) s;
  handle_option      Input.Fitcusp.(of_int   , write) f;
  handle_option    Input.Time_step.(of_float , write) ts;
  handle_option     Input.Walk_num.(of_int   , write) w;
  handle_option Input.Walk_num_tot.(of_int   , write) wt;
  handle_option Input.CI_threshold.(of_float , write) n;


  let fields = 
   [ 
     Stop_time    ;
     Block_time   ;
     Method       ;
     Ref_energy   ;
     Sampling     ;
     Time_step    ;
     Walk_num     ;
     Walk_num_tot ;
     Fitcusp      ;
     CI_threshold ;
     Jastrow_type ;
     Properties   ;
   ]
  in

  if (!interactive) then
    begin
      let temp_filename = 
          create_temp_file ezfio_filename fields
      in
      let () =
        match input with
        | Some filename ->
          begin
            if (not !interactive) then
               failwith "Input file not allowed with command line arguments"
            else
              begin
                Printf.sprintf "cp %s %s" filename temp_filename
                |> Sys.command_exn ;
              end
          end
        | None          -> 
          begin
            (* Open the temp file with external editor *)
            let editor =
              match Sys.getenv "EDITOR" with
              | Some editor -> editor
              | None -> "vi"
            in
            Printf.sprintf "%s %s ; tput sgr0 2> /dev/null" editor temp_filename 
            |> Sys.command_exn 
          end
      in
     
      (* Re-read the temp file *)
      let re_data =
         Str.regexp "   .+ *$"
      and re_prop =
         Str.regexp "([ xX]) .*$"
      and raw_data =
        In_channel.with_file temp_filename ~f:In_channel.input_lines
      in
      let data = 
        ( List.filter raw_data ~f:(fun x -> Str.string_match re_data x 0)
          |> List.map ~f:String.strip ) @
        [
        List.filter raw_data ~f:(fun x -> Str.string_match re_prop x 0)
        |> List.map ~f:String.strip
        |> String.concat ~sep:"\n" ]
      in
      let open Input in
      List.iter2_exn data fields ~f:(fun s f -> 
       try 
         begin
            match f with
            | Stop_time    ->    Stop_time.(of_string s |> write)
            | Fitcusp      ->      Fitcusp.(of_string s |> write)
            | Block_time   ->   Block_time.(of_string s |> write)
            | Method       ->       Method.(of_string s |> write)
            | Ref_energy   ->   Ref_energy.(of_string s |> write)
            | Sampling     ->     Sampling.(of_string s |> write)
            | Time_step    ->    Time_step.(of_string s |> write)
            | Walk_num     ->     Walk_num.(of_string s |> write)
            | Walk_num_tot -> Walk_num_tot.(of_string s |> write)
            | CI_threshold -> CI_threshold.(of_string s |> write)
            | Jastrow_type -> Jastrow_type.(of_string s |> write)
            | Properties   ->   Properties.(of_string s |> write)
         end
       with
       | Failure msg -> Printf.eprintf "%s\n" msg
      );
     
      (* Remove temp_file *)
      Sys.remove temp_filename;

    end
  ;

  if c then
      begin
        let dirname = 
          Filename.concat (Filename.concat ezfio_filename "blocks") (Md5.hash ()) 
        in
        let rec clean_dir y =
          match Sys.is_directory y with
          | `Yes ->
            Sys.ls_dir y
            |> List.map ~f:(Filename.concat y)
            |> List.iter ~f:(function x ->
                match ( Sys.is_directory x, Sys.is_file x ) with
                | (`Yes, _) -> clean_dir x
                | (_, `Yes) -> Sys.remove x
                | (_,_)     -> ()
            );
            Unix.rmdir y
          | `Unknown
          | `No -> ()
        in clean_dir dirname;
        Printf.printf "Blocks cleared\n"
      end
  ;

  Input.validate ();
  Md5.reset_hash ();
  write_input_in_ezfio ezfio_filename fields


let spec =
  let open Command.Spec in
  empty
  +> flag "c"  no_arg
     ~doc:(" Clear blocks")
  +> flag "f"  (optional int)
     ~doc:("0|1 "^Input.Fitcusp.doc)
  +> flag "t"  (optional int)
     ~doc:("seconds "^Input.Stop_time.doc)
  +> flag "l"  (optional int)
     ~doc:("seconds "^Input.Block_time.doc)
  +> flag "m"  (optional string)
     ~doc:("method "^Input.Method.doc)
  +> flag "e"  (optional float)
     ~doc:("energy "^Input.Ref_energy.doc)
  +> flag "s"  (optional string)
     ~doc:("sampling "^Input.Sampling.doc)
  +> flag "ts"  (optional float)
     ~doc:("time_step "^Input.Time_step.doc)
  +> flag "w"  (optional int)
     ~doc:("walk_num "^Input.Walk_num.doc)
  +> flag "wt"  (optional int)
     ~doc:("walk_num_tot "^Input.Walk_num_tot.doc)
  +> flag "n"  (optional float)
     ~doc:("norm "^Input.CI_threshold.doc)
  +> flag "j"  (optional string)
     ~doc:("jastrow_type "^Input.Jastrow_type.doc)
  +> anon ("ezfio_file" %: string)
  +> anon (maybe ("input" %: string))
;;

let command =
    Command.basic
    ~summary: "Edit input data"
    ~readme:(fun () ->
      "
Edit input data
      ")
    spec
    (fun c f t l m e s ts w wt n j ezfio_file input () -> 
      run ~c ?f ?t ?l ?m ?e ?s ?ts ?w ?wt ?n ?j ?input ezfio_file )




