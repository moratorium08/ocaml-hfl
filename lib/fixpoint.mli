(** Module for fixpoint *)

type t = Least | Greatest


val equal : t -> t -> Ppx_deriving_runtime.bool
val compare : t -> t -> Ppx_deriving_runtime.int
val pp :
  Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val iter : t -> unit
val map : t -> t
val fold : 'a -> t -> 'a
val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t

(** Converts least to greatst and vice versa *)
val flip_fixpoint : t -> t
