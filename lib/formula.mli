(** Module for (arithemetic) formulas *)

type pred = Eq | Neq | Le | Ge | Lt | Gt

(* Derived functions *)
val equal_pred : pred -> pred -> bool
val compare_pred : pred -> pred -> int
val pp_pred : Format.formatter -> pred -> unit
val show_pred : pred -> string
val iter_pred : pred -> unit
val map_pred : pred -> pred
val fold_pred : 'a -> pred -> 'a
val pred_of_sexp : Sexplib0.Sexp.t -> pred
val sexp_of_pred : pred -> Sexplib0.Sexp.t

(** Formula parametrized by variable type (['bvar]) and arith type (['avar]) *)
type ('bvar, 'avar) gen_t =
    Bool of bool
  | Var of 'bvar
  | Or of ('bvar, 'avar) gen_t list
  | And of ('bvar, 'avar) gen_t list
  | Pred of pred * 'avar Arith.gen_t list


(** Derived functions: *)

val equal_gen_t :
  ('bvar -> 'bvar -> bool) ->
  ('avar -> 'avar -> bool) ->
  ('bvar, 'avar) gen_t -> ('bvar, 'avar) gen_t -> bool
val compare_gen_t :
  ('bvar -> 'bvar -> int) ->
  ('avar -> 'avar -> int) ->
  ('bvar, 'avar) gen_t -> ('bvar, 'avar) gen_t -> int
val pp_gen_t :
  (Format.formatter -> 'bvar -> unit) ->
  (Format.formatter -> 'avar -> unit) ->
  Format.formatter -> ('bvar, 'avar) gen_t -> unit
val show_gen_t :
  (Format.formatter -> 'bvar -> unit) ->
  (Format.formatter -> 'avar -> unit) ->
  ('bvar, 'avar) gen_t -> string
val iter_gen_t : ('bvar -> unit) -> ('avar -> unit) -> ('bvar, 'avar) gen_t -> unit
val map_gen_t : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) gen_t -> ('b, 'd) gen_t
val fold_gen_t :
  ('a -> 'b -> 'a) -> ('a -> 'c -> 'a) -> 'a -> ('b, 'c) gen_t -> 'a
val gen_t_of_sexp :
  (Sexplib0.Sexp.t -> 'bvar) ->
  (Sexplib0.Sexp.t -> 'avar) -> Sexplib0.Sexp.t -> ('bvar, 'avar) gen_t
val sexp_of_gen_t :
  ('bvar -> Sexplib0.Sexp.t) ->
  ('avar -> Sexplib0.Sexp.t) -> ('bvar, 'avar) gen_t -> Sexplib0.Sexp.t

val negate_pred : pred -> pred

module Void = Util.Void

(** Type for formuals *)
type t = (Void.t, [ `Int ] Id.t) gen_t

(** Derived functions: *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string
val iter : 'a -> unit
val map : 'a -> 'a
val fold : 'a -> 'b -> 'a
val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t

val hash : t -> int

(** Constructors: *)

val mk_bool : bool -> ('a, 'b) gen_t
val mk_var : 'a -> ('a, 'b) gen_t
val mk_and : ('a, 'b) gen_t -> ('a, 'b) gen_t -> ('a, 'b) gen_t
val mk_ands : ('a, 'b) gen_t list -> ('a, 'b) gen_t
val mk_or : ('a, 'b) gen_t -> ('a, 'b) gen_t -> ('a, 'b) gen_t
val mk_ors : ('a, 'b) gen_t list -> ('a, 'b) gen_t
val mk_pred : pred -> 'a Arith.gen_t list -> ('b, 'a) gen_t
val mk_not' : ('bvar -> 'bvar) -> ('bvar, 'a) gen_t -> ('bvar, 'a) gen_t
val mk_not : (Void.t, 'a) gen_t -> (Void.t, 'a) gen_t
val mk_implies :
  (Void.t, 'a) gen_t -> (Void.t, 'a) gen_t -> (Void.t, 'a) gen_t

val to_DNF :
  ('var, 'arith) gen_t -> ('var, 'arith) gen_t list list

(** Sets of free variables *)
val fvs : ('bvar, 'avar) gen_t -> 'bvar list * 'avar list

val lift : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

val simplify_pred : pred -> 'a Arith.gen_t List.t -> bool option
