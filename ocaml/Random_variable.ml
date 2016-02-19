open Core.Std
open Qptypes

type t = 
    { property  :  Property.t ;
      data      :  Block.t list;
    }


module Average = struct
  include Sample 
end

module Error = struct
  include Sample
end

module Variance = struct
  include Sample
end

module Skewness: sig
  type t 
  val to_float : t -> float
  val of_float : float -> t
  val to_string : t -> string
end = struct
  type t = float 
  let to_string = Float.to_string
  let to_float x = x
  let of_float x = x
end

module Kurtosis: sig
  type t 
  val to_float : t -> float
  val of_float : float -> t
  val to_string : t -> string
end = struct
  type t = float 
  let to_string = Float.to_string
  let to_float x = x
  let of_float x = x
end

module GaussianDist: sig
  type t 
  val create : mu:Average.t -> sigma2:Variance.t -> t
  val eval : g:t -> x:float -> float
end = struct
  type t = { mu: Average.t ; sigma2: Variance.t } 
  let create ~mu ~sigma2 =
    { mu ; sigma2 }
  let eval ~g ~x =
    let { mu ; sigma2 } = 
      g
    in
    let mu = 
      Average.to_float mu
    and sigma2 =
      Variance.to_float sigma2
    in
    let x2 =
      (x -. mu) *. ( x -. mu) /. sigma2
    in
    let pi = 
      acos (-1.)
    in
    let c = 
      1. /. (sqrt (sigma2 *. (pi +. pi)))
    in
    c *. exp ( -0.5 *. x2)

end



(** Build from raw data. Range values are given in percent. *)
let of_raw_data ?(locked=true) ~range property =
    let data = 
         Block.raw_data ~locked ()
         |> List.filter ~f:(fun x -> x.Block.property = property)
    in

    let data_in_range rmin rmax =

        let total_weight =
            List.fold_left data ~init:0. ~f:(fun accu x ->
                (Weight.to_float x.Block.weight) +. accu
            )
        in
        
        let wmin, wmax =
          rmin *. total_weight *. 0.01,
          rmax *. total_weight *. 0.01
        in

        let (_, new_data) =
            List.fold_left data ~init:(0.,[]) ~f:(fun (wsum, l) x ->
                if (wsum > wmax) then
                  (wsum,l)
                else
                  begin
                    let wsum_new =
                        wsum +. (Weight.to_float x.Block.weight)
                    in
                    if (wsum_new > wmin) then
                        (wsum_new, x::l)
                    else
                        (wsum_new, l)
                  end 
              )
        in
        List.rev new_data
    in

    let result = 
      match range with
      | (0.,100.)   -> { property ; data }
      | (rmin,rmax) -> { property ; data=data_in_range rmin rmax }
    in
    result



(** Compute average *)
let average { property ; data } =
  if Property.is_scalar property then
    let (num,denom) = 
      List.fold ~init:(0., 0.) ~f:(fun (an, ad) x ->
        let num =
          (Weight.to_float x.Block.weight) *. (Sample.to_float x.Block.value)
        and den =
          (Weight.to_float x.Block.weight)
        in (an +. num, ad +. den)
      ) data
    in 
    num /. denom
    |> Average.of_float
  else
    let dim =
      match data with
      | [] -> 1
      | x :: tl -> Sample.dimension x.Block.value
    in
    let (num,denom) = 
      List.fold ~init:(Array.create ~len:dim 0. , 0.) ~f:(fun (an, ad) x ->
        let num =
          Array.map (Sample.to_float_array x.Block.value) ~f:(fun y -> 
          (Weight.to_float x.Block.weight) *. y)
        and den = (Weight.to_float x.Block.weight)
        in (
          Array.mapi an ~f:(fun i y -> y +. num.(i)) , 
          ad +. den)
      ) data
    in 
    let denom_inv =
      1. /. denom
    in
    Array.map num ~f:(fun x -> x *. denom_inv)
    |> Average.of_float_array ~dim



(** Compute sum (for CPU/Wall time) *)
let sum { property ; data } =
  List.fold data ~init:0. ~f:(fun accu x ->
       let num = (Weight.to_float x.Block.weight) *. (Sample.to_float x.Block.value)
       in accu +. num
     ) 



(** Calculation of the average and error bar *)
let ave_error { property ; data } =

  let rec loop ~sum ~avsq ~ansum ~avsum ~n ?idx = function 
    | [] -> 
      begin
        if (n > 0.) then
           ( Average.of_float (sum /. ansum),
             Some (Error.of_float (sqrt ( Float.abs ( avsq /.( ansum *. n)))) ))
        else
          ( Average.of_float (sum /. ansum), None)
      end
    | (x,w) :: tail ->
      begin
        let avcu0 =
          avsum /. ansum
        in
        let xw =
          x *. w
        in
        let ansum, avsum, sum =
          ansum +. w ,
          avsum +. xw ,
          sum   +. xw
        in
        loop tail
          ~sum:sum
          ~avsq:(avsq +. (1. -. (w /. ansum)) *. (x -. avcu0)
                  *. (x -. avcu0) *. w)
          ~avsum:avsum 
          ~ansum:ansum
          ~n:(n +. 1.)  
      end
  in

  let ave_error_scalar = function
    | [] -> (Average.of_float 0., None)
    | (x,w) :: tail ->
        loop tail
            ~sum:(x *. w)
            ~avsq:0.
            ~ansum:w
            ~avsum:(x *. w)
            ~n:0.
  in

  if (Property.is_scalar property) then
    List.map  data ~f:(fun x ->
      (Sample.to_float  x.Block.value,
       Weight.to_float  x.Block.weight)
    )
    |> ave_error_scalar 
  else
    match data with
    | [] -> (Average.of_float 0., None)
    | head::tail as list_of_samples ->
      let dim =
        head.Block.value
        |> Sample.dimension
      in
      let result = 
        Array.init dim ~f:(fun idx ->
          List.map  list_of_samples  ~f:(fun x ->
            (Sample.to_float  ~idx  x.Block.value,
             Weight.to_float        x.Block.weight)
          )
          |> ave_error_scalar
        )
      in
      ( Array.map result ~f:(fun (x,_) -> Average.to_float x) 
        |> Average.of_float_array ~dim , 
        if (Array.length result < 2) then
          None
        else
          Some (Array.map result ~f:(function 
            | (_,Some y) -> Error.to_float y
            | (_,None)   -> 0.)
          |> Average.of_float_array ~dim)
      ) 




(** Fold function for block values *)
let fold_blocks ~f { property ; data } =
  let init = match List.hd data with
  | None -> 0.
  | Some block -> Sample.to_float block.Block.value
  in
  List.fold_left data ~init:init ~f:(fun accu block ->
    let x = Sample.to_float block.Block.value
    in f accu x
  ) 



(** Convergence plot *)
let convergence { property ; data } =

  let rec loop ~sum ~avsq ~ansum ~avsum ~n ~accu = function 
    | [] -> List.rev accu
    | head :: tail ->
      begin
        let x = Sample.to_float head.Block.value
        and w = Weight.to_float head.Block.weight
        and avcu0 = avsum /. ansum
        in
        let xw = x *. w
        in
        let ansum = ansum +. w
        and avsum = avsum +. xw
        and sum   = sum   +. xw
        in
        let accu = 
          if (n > 0.) then
            (sum /. ansum, sqrt ( Float.abs ( avsq /.( ansum *. n))))::accu
          else
            (sum /. ansum, 0.)::accu
        in
        loop tail
          ~sum:sum
          ~avsq:(avsq +. (1. -. (w /. ansum)) *. (x -. avcu0)
                  *. (x -. avcu0) *. w)
          ~avsum:avsum 
          ~ansum:ansum
          ~n:(n +. 1.)  
          ~accu:accu
      end
  in
  match data with
  | [] -> []
  | head :: tail ->
    begin
      let x = Sample.to_float head.Block.value
      and w = Weight.to_float head.Block.weight
      in
      let s = x *. w in
      loop tail
          ~sum:s
          ~avsq:0.
          ~ansum:w
          ~avsum:s
          ~n:0.
          ~accu:[ (s /. w, 0.) ]
    end


let rev_convergence { property ; data } =
  let p = { property=property ; data = List.rev data } in
  convergence p
  |> List.rev



(** Min and max of block *)
let min_block =
  fold_blocks ~f:(fun accu x ->
    if (x < accu) then x
    else accu
  )


let max_block =
  fold_blocks ~f:(fun accu x ->
    if (x > accu) then x
    else accu
  )



(** Create a hash table for merging *)
let create_hash ~hashable ~create_key ?(update_block_id=(fun x->x)) t =
  let table = Hashtbl.create ~hashable:hashable ()
  in
  List.iter t.data ~f:(fun block ->
    let key = create_key block
    in
    let open Block in
      Hashtbl.change table key (function 
      | Some current ->
          let wc, wb =
            Weight.to_float current.weight,
            Weight.to_float block.weight
          in
          let sw =
            wc +. wb
          in
          if (Property.is_scalar current.property) then
            let vc, vb = 
              Sample.to_float current.value,
              Sample.to_float block.value
            in Some
            {  property = current.property ;
               weight   = Weight.of_float sw ;
               value    = Sample.of_float ((wc *. vc +. wb *. vb) /. sw);
               block_id = update_block_id block.block_id;
               pid      = block.pid ;
               compute_node = block.compute_node;
            }
          else
            let vc, vb = 
              Sample.to_float_array current.value,
              Sample.to_float_array block.value
            and dim = 
              Sample.dimension current.value
            in Some
            {  property = current.property ;
               weight   = Weight.of_float sw ;
               value    = 
                 Array.init dim ~f:(fun i -> ((wc *. vc.(i) +. wb *. vb.(i)) /. sw))
                 |> Sample.of_float_array ~dim ;
               block_id = update_block_id block.block_id;
               pid      = block.pid ;
               compute_node = block.compute_node;
            }
      | None -> Some
          {  property = block.property ;
            weight   = block.weight;
            value    = block.value ;
            block_id = update_block_id block.block_id;
            pid      = block.pid ;
            compute_node = block.compute_node;
          }
        )
  );
  table



(** Genergic merge function *)
let merge ~hashable ~create_key ?update_block_id t =
  let table = create_hash ~hashable:hashable ~create_key:create_key 
   ?update_block_id:update_block_id t
  in
  { property = t.property ;
    data = Hashtbl.to_alist table
    |>  List.sort ~cmp:(fun x y ->
        if (x>y) then 1
        else if (x<y) then -1
        else 0)
    |>  List.map ~f:(fun (x,y) -> y)
  }



(** Merge per block id *)
let merge_per_block_id =
  merge 
   ~hashable:Int.hashable
   ~create_key:(fun block -> Block_id.to_int block.Block.block_id)


(** Merge per compute_node *)
let merge_per_compute_node =
  merge
   ~hashable:String.hashable
   ~create_key:(fun block -> 
      Printf.sprintf "%s" 
       (Compute_node.to_string block.Block.compute_node) )



(** Merge per Compute_node and PID *)
let merge_per_compute_node_and_pid =
  merge
   ~hashable:String.hashable
   ~create_key:(fun block -> 
      Printf.sprintf "%s %10.10d" 
       (Compute_node.to_string block.Block.compute_node)
       (Pid.to_int block.Block.pid) )



(** Merge per Compute_node and BlockId *)
let merge_per_compute_node_and_block_id =
  merge
   ~hashable:String.hashable
   ~create_key:(fun block -> 
      Printf.sprintf "%s %10.10d" 
       (Compute_node.to_string block.Block.compute_node)
       (Block_id.to_int block.Block.block_id) )




(** Merge two consecutive blocks *)
let compress =
  merge
   ~hashable:String.hashable
   ~create_key:(fun block -> 
      Printf.sprintf "%s %10.10d" (Compute_node.to_string block.Block.compute_node)
      (((Block_id.to_int block.Block.block_id)+1)/2))
   ~update_block_id:(fun block_id -> 
      ((Block_id.to_int block_id)+1)/2
      |> Block_id.of_int )

  


(** Last value on each compute node (for wall_time) *)
let max_value_per_compute_node t =
  let table = Hashtbl.create ~hashable:String.hashable ()
  in
  let create_key block =
      Printf.sprintf "%s %10.10d" 
       (Compute_node.to_string block.Block.compute_node)
       (Pid.to_int block.Block.pid) 
  in
  List.iter t.data ~f:(fun block ->
    let key = create_key block
    in
    let open Block in
    Hashtbl.change table key (function 
    | Some current ->
        let vc = Sample.to_float current.value
        and vb = Sample.to_float block.value
        in
        if (vc > vb) then
          Some current
        else
          Some block
    | None -> Some block
      )
  );
  { property = t.property ;
    data = Hashtbl.to_alist table
    |>  List.sort ~cmp:(fun x y ->
        if (x>y) then 1
        else if (x<y) then -1
        else 0)
    |>  List.map ~f:(fun (x,y) -> y)
  }




(** String representation *)
let to_string p = 
  match p.property with
  | Property.Cpu   ->  Printf.sprintf "%s" (Time.Span.to_string (Time.Span.of_sec (sum p)))
  | Property.Wall  ->  Printf.sprintf "%s" (Time.Span.to_string (Time.Span.of_sec (sum (max_value_per_compute_node p))))
  | Property.Accep ->  Printf.sprintf "%16.10f" (average p |> Average.to_float)
  | _ -> 
    begin
      if Property.is_scalar p.property then
          match ave_error p with
          | (ave, Some error) -> 
              let (ave, error) = 
                Average.to_float ave, 
                Error.to_float error
              in
              Printf.sprintf "%16.10f +/- %16.10f" ave error
          | (ave, None) -> 
              let ave = 
                Average.to_float ave
              in
              Printf.sprintf "%16.10f" ave
      else
          match ave_error p with
          | (ave, Some error) -> 
              let idxmax =
                Average.dimension ave
              in
              let rec f accu idx =
                if (idx < idxmax) then
                    let (ave, error) = 
                      Average.to_float ~idx ave, 
                      Error.to_float ~idx error
                    in
                    let s =
                      Printf.sprintf "%8d :  %16.10f +/- %16.10f ;\n" (idx+1) ave error
                    in
                    f (accu ^ s) (idx+1)
                else
                    accu
              in
              (f "[ \n" 0) ^ " ]"
          | (ave, None) -> 
              Average.to_float ave
              |> Printf.sprintf "%16.10f" 
    end




(** Compress block files : Merge all the blocks computed on the same host *)
let compress_files () =

  Block._raw_data := None;

  let properties =
    Lazy.force Block.properties
  in
  
  (* Create temporary file *)
  let dir_name =
    Block.dir_name
  in

  let dir_name = 
    Lazy.force dir_name
  in
  let files = 
    Sys.ls_dir dir_name
    |> List.filter ~f:(fun x -> 
         match String.substr_index ~pattern:"locked" x with
         | Some x -> false
         | None -> true
       )
    |> List.map ~f:(fun x -> dir_name^x)
  in

  let out_channel_dir = 
    Filename.temp_dir ~in_dir:(!Ezfio.ezfio_filename ^ "/blocks/") "qmc" "" 
  in

  let out_channel_name = 
    let hostname =
      Lazy.force Qmcchem_config.hostname 
    and suffix =
     Unix.getpid ()
     |> Pid.to_string 
    in
    String.concat [ hostname ; "." ; suffix ]
  in

  let block_channel = 
    Out_channel.create (out_channel_dir ^ out_channel_name)
  in

  List.iter properties ~f:(fun p ->
    let l = 
      match p with
        | Property.Cpu 
        | Property.Accep ->
          of_raw_data ~locked:false ~range:(0.,100.) p
            |> merge_per_compute_node
        | Property.Wall  -> 
          of_raw_data ~locked:false ~range:(0.,100.) p
            |> max_value_per_compute_node 
        | _     ->
          of_raw_data ~locked:false ~range:(0.,100.) p
            |> merge_per_compute_node_and_block_id
    in
    List.iter l.data ~f:(fun x ->
      Out_channel.output_string block_channel (Block.to_string x);
      Out_channel.output_char   block_channel '\n';
    );
  ); 
  Out_channel.close block_channel;

  List.iter files ~f:Unix.remove ;
  Unix.rename ~src:(out_channel_dir^out_channel_name)  ~dst:(dir_name^out_channel_name);
  Unix.rmdir  out_channel_dir



(** Autocovariance function (not weighted) *)
let autocovariance { property ; data } =
  let ave =
    average { property ; data }
    |> Average.to_float
  and data =
    match (merge_per_block_id { property ; data })
    with { property ; data } -> Array.of_list data
  in
  let x_t = 
     Array.map ~f:(fun x -> (Sample.to_float x.Block.value) -. ave) data
  in
  let f i = 
    let denom = 
      if (i > 1) then (Float.of_int i) else 1.
    in
    let r = 
      Array.sub ~pos:0 ~len:i x_t
      |> Array.fold ~init:0. ~f:(fun accu x ->
        accu +. x *. x_t.(i)) 
    in
    r /. denom
  in
  Array.init ~f (Array.length data)
  |> Array.to_list



(** Computes the first 4 centered cumulants (zero mean) *)
let centered_cumulants { property ; data } =
  let ave =
    average { property ; data }
    |> Average.to_float
  in
  let centered_data =
     List.map ~f:(fun x -> 
      ( (Weight.to_float x.Block.weight), 
        (Sample.to_float x.Block.value) -. ave )
      ) 
     data
  in
  let var  = 
     let (num, denom) = 
     List.fold ~init:(0., 0.) ~f:(fun (a2, ad) (w,x) ->
       let x2 = x *. x
       in
       let var = w *. x2
       and den = w
       in (a2 +. var, ad +. den)
     ) centered_data
     in num /. denom
  in 
  let centered_data =
     let sigma_inv =
       1. /. (sqrt var)
     in
     List.map ~f:(fun x -> 
      ( (Weight.to_float x.Block.weight), 
        ( (Sample.to_float x.Block.value) -. ave ) *. sigma_inv )
      ) 
     data
  in
  let (cum3,cum4) = 
     let (cum3, cum4, denom) =
     List.fold ~init:(0., 0., 0.) ~f:(fun (a3, a4, ad) (w,x) ->
       let x2 = x *. x
       in
       let cum3 = w *. x2 *. x
       and cum4 = w *. x2 *. x2
       and den = w
       in (a3 +. cum3, a4 +. cum4, ad +. den)
     ) centered_data
     in 
     ( cum3 /. denom, cum4 /. denom -. 3. )
  in 
  [| ave ; var  ; cum3 ;  cum4 |]




(** Computes a histogram *)
let histogram { property ; data } =
  let min, max = 
    (min_block { property ; data }),
    (max_block { property ; data })
  in
  let length = 
    max -. min
  and n =
    List.length data
    |> Float.of_int
    |> sqrt
  in
  let delta_x = 
    length /. (n-.1.)
  and result =
    Array.init ~f:(fun _ -> 0.) (Int.of_float (n +. 1.))
  in
  List.iter ~f:(fun x ->
    let w =
      (Weight.to_float x.Block.weight)
    and x = 
      (Sample.to_float x.Block.value)
    in
    let i =
      (x -. min) /. delta_x +. 0.5
      |> Float.to_int
    in
    result.(i) <- result.(i) +. w
  ) data
  ;
  let norm = 
    1. /. ( delta_x *. (
      Array.fold ~init:0. ~f:(fun accu x -> accu +. x) result
    ) )
  in
  Array.mapi ~f:(fun i x -> (min +. (Float.of_int i)*.delta_x, x *. norm) ) result
  |> Array.to_list


