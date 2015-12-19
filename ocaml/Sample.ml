open Core.Std

type t = 
  | One_dimensional  of float
  | Multidimensional of (float array * int)
with sexp

let dimension = function
  | One_dimensional _ -> 1
  | Multidimensional (_,d) -> d

let to_float ?idx x = 
  match (idx,x) with
  | None  , One_dimensional x
  | Some 0, One_dimensional x  -> x 
  | Some i, One_dimensional x  ->
      failwith "Index should not be specified in One_dimensional"
  | None  , Multidimensional (x,_) -> x.(0)
  | Some i, Multidimensional (x,s) when i < s  -> x.(i)
  | Some i, Multidimensional (x,s) -> 
      Printf.sprintf "Index out of bounds in Multidimensional 
      %d not in [0,%d[ " i s
      |> failwith

let to_float_array = function 
  | One_dimensional _ -> failwith "Should be Multidimensional"
  | Multidimensional (x,_) -> x

let of_float x = 
  One_dimensional x

let of_float_array ~dim x = 
  if (Array.length x) <> dim then
    failwith "Inconsistent array size in of_float_array"
  else
    match dim with
    | 1 -> One_dimensional x.(0)
    | _ -> Multidimensional (x, dim)

let to_string = function
  | One_dimensional  x -> Float.to_string x
  | Multidimensional (x,_) -> 
      Array.map x ~f:Float.to_string
      |> String.concat_array ~sep:" " 
      |> Printf.sprintf "%s"

