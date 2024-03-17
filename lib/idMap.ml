(* NOTE Information of type will be lost! *)
open Base
include Map.M(Id.Key)

let empty = Map.empty (module Id.Key)
let singleton : 'a Id.t -> 'x -> 'x t =
  fun v x ->
    Map.singleton (module Id.Key) (Id.remove_ty v) x
let add : 'x t -> 'a Id.t -> 'x -> 'x t =
  fun env v data ->
    Map.add_exn env ~key:(Id.remove_ty v) ~data
let set : 'x t -> 'a Id.t -> 'x -> 'x t =
  fun env v data ->
    Map.set env ~key:(Id.remove_ty v) ~data
let find : 'x t -> 'a Id.t -> 'x option =
  fun map v -> Map.find map (Id.remove_ty v)
let lookup : 'x t -> 'a Id.t -> 'x =
  fun map v -> Map.find_exn map (Id.remove_ty v)
let remove : 'x t -> 'a Id.t -> 'x t =
  fun map v -> Map.remove map (Id.remove_ty v)
let replace : 'x t ->  'a Id.t -> 'x -> 'x t =
  fun env v data ->
  let env = remove env v in
  add env v data
let of_list : ('a Id.t * 'x) list -> 'x t =
  fun vxs -> Map.of_alist_exn (module Id.Key) @@ List.map ~f:(fun (v,x) -> (Id.remove_ty v, x)) vxs
let to_alist = Map.to_alist
let iter_keys = Map.iter_keys
let map = Map.map
