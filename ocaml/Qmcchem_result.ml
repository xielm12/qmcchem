open Core.Std
open Qptypes

(** Display a table that can be plotted by gnuplot *)
let display_table property =
  let p = Property.of_string property
  |> Random_variable.of_raw_data
  in
  let  conv = Random_variable.convergence p
  and rconv = Random_variable.rev_convergence p
  and data  = p.Random_variable.data
  in
  let results = 
  List.map2_exn conv rconv ~f:(fun (val1, err1) (val2,err2) -> (val1, err1, val2, err2))
  in
  List.iter2_exn results data ~f:(fun (val1, err1, val2, err2) block ->
     Printf.printf "%10.6f  %10.6f    %10.6f  %10.6f   %10.6f\n"
     val1 err1 val2 err2 (Sample.to_float block.Block.value)
  )
;;


(** Display a convergence plot of the requested property *)
let display_plot property =
  print_string ("display_plot "^property^".\n")
;;


(** Display a convergence table of the error *)
let display_err_convergence property =
  let p =
    Property.of_string property
    |> Random_variable.of_raw_data
  in
  let rec aux n p = 
      match Random_variable.ave_error p with
      | (ave, Some error) ->
          let (ave, error) = 
            Random_variable.Average.to_float ave, 
            Random_variable.Error.to_float error 
          in
          Printf.printf "%10d  %16.10f %16.10f\n" n ave error ;
          begin
            if ((3*n) < (List.length p.Random_variable.data)) then
              let new_p = 
                 Random_variable.compress p
              in
              aux (n+n) new_p
          end
      | (ave, None) -> ()
  in
  aux 1 p
;;

(** Display the centered cumulants of a property *)
let display_cumulants property =
  let p =
    Property.of_string property
    |> Random_variable.of_raw_data
  in
  let cum =
    Random_variable.centered_cumulants p
  in
  Printf.printf "Average     = %16.10f\n" cum.(0);
  Printf.printf "Variance    = %16.10f\n" cum.(1);
  Printf.printf "Centered k3 = %16.10f\n" cum.(2);
  Printf.printf "Centered k4 = %16.10f\n" cum.(3);
  print_newline ();
  let n = 1. /. 12. *. cum.(2) *. cum.(2) +.
          1. /. 48. *. cum.(3) *. cum.(3)
  in
  Printf.printf "Non-gaussianity = %16.10f\n" n
;;


(** Display a table for the autocovariance of the property *)
let display_autocovariance property =
  let p = 
    Property.of_string property
    |> Random_variable.of_raw_data
  in
  Random_variable.autocovariance p
  |> List.iteri ~f:(fun i x ->
      Printf.printf "%10d  %16.10f\n" i x)
;;

(** Display a histogram of the property *)
let display_histogram property =
  let p = 
    Property.of_string property
    |> Random_variable.of_raw_data
  in
  let histo = 
    Random_variable.histogram p

  in
  let g = 
    Random_variable.GaussianDist.create
      ~mu:(Random_variable.average p)
      ~sigma2:((Random_variable.centered_cumulants p).(1)
                |> Random_variable.Variance.of_float)
  in
  let g =
    Random_variable.GaussianDist.eval ~g
  in
  List.iter histo ~f:( fun (x,y) -> 
      Printf.printf "%16.10f  %16.10f  %16.10f\n" x y (g ~x))
    (*
  and sigma2 = 
    (Random_variable.centered_cumulants p).(1) 
  and pi =
    acos(-1.)
  in
  let one_over_2sigma2 =
    1. /. ( 2. *. sigma2 )
  and mu =
    Random_variable.average p
  and norm =
    1. /. (sqrt(sigma2 *. 2.*.pi))
  in
  List.map histo ~f:(fun (x,y) -> 
    let g =
      norm *. exp(-.((x-.mu)*.(x-.mu)*.one_over_2sigma2))
    in
    (x,y,g)
  )
  |>  List.iter ~f:(fun (x,y,g) ->
      Printf.printf "%16.10f  %16.10f  %16.10f\n" x y g)
      *)
;;



(** Display a summary of all the cmoputed quantities *)
let display_summary () =
  
  let properties =
    Lazy.force Block.properties
  and print_property property = 
    let p = Random_variable.of_raw_data property
    in
    Printf.printf "%20s : %s\n"
     (Property.to_string property)
     (Random_variable.to_string p)
  in
  List.iter properties ~f:print_property ;


  let cpu =
    Random_variable.of_raw_data Property.Cpu
    |> Random_variable.sum
  and wall =
    Random_variable.of_raw_data Property.Wall
    |> Random_variable.max_value_per_compute_node
    |> Random_variable.sum
  in

  let speedup =
    cpu /. wall
  in
  Printf.printf "%20s : %10.2f x\n" "Speedup" speedup;
;;


let run ?a ?c ?e ?h ?t ?p ezfio_file =

  Qputils.set_ezfio_filename ezfio_file;
  let f (x,func) =
    match x with
    | Some property -> func property 
    | None -> ()
  in

  let l = 
    [ (a, display_autocovariance) ;
      (c, display_cumulants) ;
      (e, display_err_convergence) ;
      (h, display_histogram) ;
      (p, display_plot) ;
      (t, display_table) ;
    ]
  in

  List.iter ~f l
  ;

  if (List.fold ~init:true ~f:(fun accu x ->
      match x with
      | (None, _) -> accu && true
      | (Some _,_) -> false
     ) l
    ) then
    display_summary ()
;;


let spec =
  let open Command.Spec in
  empty
  +> flag "a" (optional string)
     ~doc:"property Display the autcovariance function of the property"
  +> flag "c"  (optional string)
     ~doc:"property Print the centered cumulants of a property"
  +> flag "e" (optional string)
     ~doc:"property Display the convergence of the error of the property by merging blocks"
  +> flag "h" (optional string)
     ~doc:"property Display the histogram of the property blocks"
  +> flag "p" (optional string)
     ~doc:"property Display a convergence plot for a property"
  +> flag "t"  (optional string)
     ~doc:"property Print a table for the convergence of a property"
  +> anon ("ezfio_file" %: string)
;;

let command =
    Command.basic
    ~summary: "Displays the results computed in an EZFIO directory."
    ~readme:(fun () -> "Displays the results computed in an EZFIO directory.")
    spec
    (fun a c e h p t ezfio_file () -> run ?a ?c ?e ?h ?t ?p ezfio_file )
;;




