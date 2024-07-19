(** Module for types *)

(** The gramamr of type is *)
(** {math \iota ::= \mathrm{int} \mid \sigma \qquad \tau ::= \mathrm{bool} \mid \iota \to \tau } *)
(** where {m \sigma} may differ among the type systems we are considering.
    Usually, {m \sigma} contains {m \tau}.
    The type {m \mathrm{bool}} is also known as proposition type *)

(** {1 General type} *)

(** Types for function arguments. Corresponds to {m \iota} in the doc.*)
type 'ty arg = TyInt | TySigma of 'ty

(** Derived functions *)

val equal_arg : ('ty -> 'ty -> bool) -> 'ty arg -> 'ty arg -> bool
val compare_arg : ('ty -> 'ty -> int) -> 'ty arg -> 'ty arg -> int
val pp_arg : (Format.formatter -> 'ty -> unit) -> Format.formatter -> 'ty arg -> unit
val show_arg : (Format.formatter -> 'ty -> unit) -> 'ty arg -> string
val iter_arg : ('ty -> unit) -> 'ty arg -> unit
val map_arg : ('a -> 'b) -> 'a arg -> 'b arg
val fold_arg : ('a -> 'b -> 'a) -> 'a -> 'b arg -> 'a
val arg_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty arg
val sexp_of_arg : ('ty -> Sexplib0.Sexp.t) -> 'ty arg -> Sexplib0.Sexp.t

type 'annot ty =
    TyBool of 'annot
  | TyArrow of 'annot ty arg Id.t * 'annot ty

(** Derived functions *)

val equal_ty : ('annot -> 'annot -> bool) -> 'annot ty -> 'annot ty -> bool
val compare_ty : ('annot -> 'annot -> int) -> 'annot ty -> 'annot ty -> int
val pp_ty :
  (Format.formatter -> 'annot -> unit) ->
  Format.formatter -> 'annot ty -> unit
val show_ty : (Format.formatter -> 'annot -> unit) -> 'annot ty -> string
val iter_ty : ('annot -> unit) -> 'annot ty -> unit
val map_ty : ('a -> 'b) -> 'a ty -> 'b ty
val fold_ty : ('a -> 'b -> 'a) -> 'a -> 'b ty -> 'a
val ty_of_sexp : (Sexplib0.Sexp.t -> 'annot) -> Sexplib0.Sexp.t -> 'annot ty
val sexp_of_ty : ('annot -> Sexplib0.Sexp.t) -> 'annot ty -> Sexplib0.Sexp.t

(** {m \iota} where {m \sigma} is {m \tau} (with extra annotations) *)
type 'annot arg_ty = 'annot ty arg

(** Derived functions *)

val equal_arg_ty :
  ('annot -> 'annot -> bool) ->
  'annot arg_ty -> 'annot arg_ty -> bool
val compare_arg_ty :
  ('annot -> 'annot -> int) ->
  'annot arg_ty -> 'annot arg_ty -> int
val pp_arg_ty :
  (Format.formatter -> 'annot -> unit) ->
  Format.formatter -> 'annot arg_ty -> unit
val show_arg_ty :
  (Format.formatter -> 'annot -> unit) -> 'annot arg_ty -> string
val iter_arg_ty : ('annot -> unit) -> 'annot ty arg -> unit
val map_arg_ty : ('a -> 'b) -> 'a ty arg -> 'b ty arg
val fold_arg_ty : ('a -> 'b -> 'a) -> 'a -> 'b ty arg -> 'a
val arg_ty_of_sexp : (Sexplib0.Sexp.t -> 'annot) -> Sexplib0.Sexp.t -> 'annot arg_ty
val sexp_of_arg_ty : ('annot -> Sexplib0.Sexp.t) -> 'annot arg_ty -> Sexplib0.Sexp.t

val unsafe_unlift : 'annot arg_ty -> 'annot ty
val lift_arg : 'a Id.t -> 'a arg Id.t

(** {2 Non-derived functions }*)

val mk_arrows : 'annot ty arg Id.t list -> 'annot ty -> 'annot ty
val decompose_arrow : 'annot ty -> 'annot ty arg Id.t list * 'annot

val merge : ('annot -> 'annot -> 'annot) -> 'annot ty -> 'annot ty -> 'annot ty
(** [merge f ty1 ty2] "merges" the annotations in [ty1] and [ty2] using [f] under
    the assumtion that [ty1] and [ty2] have the same shape (otherwise, raises [invalid_arg]) *)

val merges : ('annot -> 'annot -> 'annot) -> 'annot ty list -> 'annot ty
(** [merges f [ty1; ...; ty_n] = ty1 ++ ty2 ++ ... ++ tyn] where [++] is [merge f]  *)


(** {1 Simple type} *)

(** Simple type is defined as general type (['annot ty]) with no extra annotations *)
type simple_ty = unit ty

(** Derived functions *)

val equal_simple_ty : simple_ty -> simple_ty -> bool
val compare_simple_ty : simple_ty -> simple_ty -> int
val pp_simple_ty : Format.formatter -> simple_ty -> unit
val show_simple_ty : simple_ty -> string
val simple_ty_of_sexp : Sexplib0.Sexp.t -> simple_ty
val sexp_of_simple_ty : simple_ty -> Sexplib0.Sexp.t

type simple_argty = simple_ty arg

(** Derived functions *)

val equal_simple_argty : simple_argty -> simple_argty -> bool
val compare_simple_argty : simple_argty -> simple_argty -> int
val pp_simple_argty : Format.formatter -> simple_argty -> unit
val show_simple_argty : simple_argty -> string
val simple_argty_of_sexp : Sexplib0.Sexp.t -> simple_argty
val sexp_of_simple_argty : simple_argty -> Sexplib0.Sexp.t

(** {2 Non-derived functions }*)

val to_simple : 'annot ty -> simple_ty
(** Deletes the annotations *)
