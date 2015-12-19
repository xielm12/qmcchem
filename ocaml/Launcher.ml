open Core.Std;;

type t =
  | Srun
  | MPI
  | Bash

let to_string = function
  | Srun     -> "srun"
  | Bash     -> "env"
  | MPI      -> Lazy.force Qmcchem_config.mpirun


(** Find the launcher for the current job scheduler *)
let find () = 

  let result = 
    match Scheduler.find () with
    | Scheduler.SLURM -> Srun
    | Scheduler.Batch
    | Scheduler.PBS
    | Scheduler.SGE ->
      if Lazy.force Qmcchem_config.has_mpirun then
        MPI
      else
        Bash
  in
  result


(** Create a file contaning the list of nodes and the number of available CPUs *)
let create_nodefile () =

  let launcher =
    find ()
  in

  let launcher_command =
    to_string launcher
  in

  let h =
    Hashtbl.create ~hashable:String.hashable ~size:1000 ()
  in

  let in_channel =
    Unix.open_process_in (launcher_command^" hostname -s")
  in
  In_channel.input_lines in_channel
  |> List.map  ~f:String.strip
  |> List.iter ~f:( fun host ->
       Hashtbl.change h host (function
        | Some x -> Some (x+1)
        | None -> Some 1
       )
     );
  match
    Unix.close_process_in in_channel
  with
  | _ -> ();


  let f = 
    match launcher with
    | MPI ->
        fun (node, n) ->
            Printf.sprintf "%s slots=%d\n" node n
    | Srun 
    | Bash -> 
        fun (node, n) ->
            Printf.sprintf "%s %d\n" node n
  in
  Hashtbl.to_alist h
  |> List.map ~f
  |> String.concat  




