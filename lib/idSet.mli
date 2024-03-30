(** Module for sets of identifiers *)

type t = (Id.Key.t, Id.Key.comparator_witness) Base.Set.t

val empty : t
val singleton : 'a Id.t -> t

val remove : t -> 'a Id.t -> t
val mem : t -> 'a Id.t -> bool
val add : t -> 'a Id.t -> t

val union : t -> t-> t
val union_list : t list -> t

val filter : t -> f:(unit Id.t -> bool) -> t

val to_list : t -> unit Id.t list
val of_list : unit Id.t list -> t
