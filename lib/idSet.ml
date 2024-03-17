open Base
include Set.M(Id.Key)
let empty = Set.empty (module Id.Key)
let singleton : 'a. 'a Id.t -> t =
  fun v -> Set.singleton (module Id.Key) (Id.remove_ty v)
let remove : 'a. t -> 'a Id.t -> t =
  fun set x -> Set.remove set (Id.remove_ty x)
let mem set x = Set.mem set (Id.remove_ty x)
let add set x = Set.add set (Id.remove_ty x)
let union = Set.union
let union_list = Set.union_list (module Id.Key)
let filter = Set.filter
let to_list = Set.to_list
let of_list = Set.of_list (module Id.Key)
