open Core.Std;;
open Qptypes;;

type t = 
  { property       : Property.t ;
    value          : Sample.t        ;
    weight         : Weight.t        ;
    compute_node   : Compute_node.t  ;
    pid            : Pid.t           ;
    block_id       : Block_id.t      ;
  }

let re =
  Str.regexp "[ |#|\n]+"

let of_string s = 

   try
     let lst =
        Str.split re s
        |> List.rev
     in
      match lst with
      | b :: pid :: c:: p :: w :: v :: [] -> Some
        { property = Property.of_string p ;
          value    = Sample.of_float (Float.of_string v) ;
          weight   = Weight.of_float (Float.of_string w) ; 
          compute_node = Compute_node.of_string c;
          pid          = Pid.of_string pid;
          block_id     = Block_id.of_int (Int.of_string b) ;
        }
      | b :: pid :: c:: p :: w :: v -> 
          let v = 
            List.rev v
            |> Array.of_list 
            |> Array.map ~f:Float.of_string
          in
          let dim = 
            Array.length v
          in
        Some
        { property = Property.of_string p ;
          value    = Sample.of_float_array ~dim v ;
          weight   = Weight.of_float (Float.of_string w) ; 
          compute_node = Compute_node.of_string c;
          pid          = Pid.of_string pid;
          block_id     = Block_id.of_int (Int.of_string b) ;
        }
      | _ -> None
   with
   | _ -> None


  
let to_string b =
  Printf.sprintf "%s %s # %s %s %s %d"
  (Sample.to_string       b.value )
  (Weight.to_float        b.weight       |> Float.to_string)
  (Property.to_string     b.property)
  (Compute_node.to_string b.compute_node)
  (Pid.to_string          b.pid)
  (Block_id.to_int        b.block_id)



let dir_name = lazy(
  let ezfio_filename = 
     Lazy.force Qputils.ezfio_filename
  in
  let md5 =
     Md5.hash ()
  in
  List.fold_right ~init:"" ~f:Filename.concat 
    [ ezfio_filename ; "blocks" ; md5 ; Filename.dir_sep ]
)


(* Fetch raw data from the EZFIO file *)
let _raw_data =
  ref None


let update_raw_data ?(locked=true) () =
  (* Create array of files to read *)
  let dir_name = 
    Lazy.force dir_name
  in
  let files = 
     let result = 
       match Sys.is_directory dir_name with
       | `Yes -> 
         begin
           Sys.readdir dir_name
           |> Array.map ~f:(fun x -> dir_name^x)
           |> Array.to_list
         end
       | _ -> []
     in
     if locked then
       result
     else
       List.filter result ~f:(fun x ->
         match String.substr_index ~pattern:"locked" x with
         | Some x -> false
         | None -> true
       )
  in

  let rec transform new_list = function 
   | [] -> new_list
   | head :: tail ->
     let head = String.strip head in
     let item = of_string head in
     match item with
       | None   -> transform new_list tail
       | Some x -> transform (x::new_list) tail
  in

  let result =
  List.map files ~f:(fun filename ->
    In_channel.with_file filename ~f:(fun in_channel ->
      In_channel.input_all in_channel)
    )
  |> String.concat 
  |> String.split_lines 
  |> List.rev
  |> transform [] 
  in
  result


let raw_data ?(locked=true) () = 
  match !_raw_data with
  | Some x -> x
  | None   ->
    let result = 
      update_raw_data ~locked ()
    in
    _raw_data := Some result;
    result



let properties = lazy (
  let set = Set.empty ~comparator:Comparator.Poly.comparator in
  List.fold (raw_data ()) ~init:set ~f:(fun s x -> Set.add s x.property)
    |> Set.to_list
)


