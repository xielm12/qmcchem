open Core.Std;;

type t = 
  | SGE
  | PBS
  | SLURM
  | Batch


let to_string = function
  | SGE   -> "SGE"
  | PBS   -> "PBS"
  | SLURM -> "SLURM"
  | Batch -> "Batch"



let find () = 
  let scheduler = 
    [  "SLURM_NODELIST" ; "PE_HOSTFILE" ; "PBS_NODEFILE" ]
    |> List.map ~f:(function x -> 
         match (Sys.getenv x) with
           | Some _ -> x
           | None -> ""
         )
    |> List.filter ~f:(function x -> x <> "")
    |> List.hd
  in
  let result = 
    match scheduler with
      | Some "SLURM_NODELIST"  -> SLURM
      | Some "PE_HOSTFILE"     -> SGE
      | Some "PBS_NODEFILE"    -> PBS
      | None                   -> Batch
      | Some x  -> failwith (Printf.sprintf "Scheduler %s not found" x)
  in
  result



