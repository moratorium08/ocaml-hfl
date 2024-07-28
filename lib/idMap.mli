(** Module for maps whose keys are identifiers *)

type 'v t = (Id.Key.t, 'v, Id.Key.comparator_witness) Base.Map.t

val empty : 'v t
val singleton : 'ty Id.t -> 'v -> 'v t

val add : 'v t -> 'ty Id.t -> 'v -> 'v t
val set : 'v t -> 'ty Id.t -> 'v -> 'v t
val find : 'v t -> 'ty Id.t -> 'v option
val lookup : 'v t -> 'ty Id.t -> 'v
val remove : 'v t -> 'ty Id.t -> 'v t
val replace : 'v t -> 'v Id.t -> 'v -> 'v t

val of_list : ('ty Id.t * 'v) list -> 'v t
val to_alist :
  ?key_order:[ `Decreasing | `Increasing ] -> 'v t -> (Id.Key.t * 'v) list

val iter_keys : 'v t -> f:(Id.Key.t -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
