(** Module for identifiers *)

(** Identifiers are typed. That is, we keep x : t
with a unique identifier representing this binding *)
(** ['ty] is typically a type of the id *)
type 'ty t = { name : string; id : int; ty : 'ty; }

(** Derived functions *)

(** Consider using [eq] instead *)
val equal : ('ty -> 'ty -> bool) -> 'ty t -> 'ty t -> bool

val compare : ('ty -> 'ty -> int) -> 'ty t -> 'ty t -> int
val pp :
  (Format.formatter -> 'ty -> unit) ->
  Format.formatter -> 'ty t -> unit

(** Consider using [to_string] instead *)
val show : (Format.formatter -> 'ty -> unit) -> 'ty t -> string
val iter : ('ty -> unit) -> 'ty t -> unit
val map : ('ty -> 'a) -> 'ty t -> 'a t
val fold : ('a -> 'ty -> 'b) -> 'a -> 'ty t -> 'b
val t_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty t
val sexp_of_t : ('ty -> Sexplib0.Sexp.t) -> 'ty t -> Sexplib0.Sexp.t

val eq : 'a t -> 'b t -> bool

(* TODO this should be hidden  *)
val gen_id : unit -> int

val to_string : 'ty t -> string
val gen : ?name:string -> 'ty-> 'ty t
val remove_ty : 'ty t -> unit t

module Key :
  sig
    type nonrec t = unit t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val compare : t -> t -> int
    val hash : t -> int
    type comparable_t = t
    type comparator_witness
    val comparator :
      (comparable_t, comparator_witness) Base__Comparator.comparator
  end
