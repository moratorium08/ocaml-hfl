(** Module for raw (= unyped) HFL(Z) formulas *)

(** This module implements the untyped version of the HFL(Z) formulas; See {!Hflz} for detailed explanation about HFL(Z) formulas
    A type checker (for simple types) based on unification algorithm is also provided.
*)

(** {1 Body Formula} *)

(** Untyped version of {!Hflz.Sugar.t} *)
type raw_hflz =
    Bool of bool
  | Var of string
  | Or of raw_hflz * raw_hflz
  | And of raw_hflz * raw_hflz
  | Abs of string * raw_hflz
  | App of raw_hflz * raw_hflz
  | Int of int
  | Op of Arith.op * raw_hflz list
  | Pred of Formula.pred * raw_hflz list
  | Forall of string * raw_hflz
  | Exists of string * raw_hflz
  | Not of raw_hflz

(** {2 Derived functions} *)

val equal_raw_hflz : raw_hflz -> raw_hflz -> bool
val compare_raw_hflz : raw_hflz -> raw_hflz -> int
val pp_raw_hflz : Format.formatter -> raw_hflz -> unit
val show_raw_hflz : raw_hflz -> string
val iter_raw_hflz : raw_hflz -> unit
val map_raw_hflz : raw_hflz -> raw_hflz
val fold_raw_hflz : 'a -> raw_hflz -> 'a
val raw_hflz_of_sexp : Sexplib0.Sexp.t -> raw_hflz
val sexp_of_raw_hflz : raw_hflz -> Sexplib0.Sexp.t

(** {2 Constructors} *)

val mk_int : int -> raw_hflz
val mk_bool : bool -> raw_hflz
val mk_var : string -> raw_hflz
val mk_op : Arith.op -> raw_hflz list -> raw_hflz
val mk_forall : string -> raw_hflz -> raw_hflz
val mk_exists : string -> raw_hflz -> raw_hflz
val mk_ands : raw_hflz list -> raw_hflz
val mk_ors : raw_hflz list -> raw_hflz
val mk_not : raw_hflz -> raw_hflz
val mk_pred : Formula.pred -> raw_hflz -> raw_hflz -> raw_hflz
val mk_app : raw_hflz -> raw_hflz -> raw_hflz
val mk_apps : raw_hflz -> raw_hflz list -> raw_hflz
val mk_abs : string -> raw_hflz -> raw_hflz
val mk_abss : string list -> raw_hflz -> raw_hflz

(** {2 Decomposers} *)

val decompose_app : raw_hflz -> raw_hflz * raw_hflz list
val decompose_abs : raw_hflz -> string list * raw_hflz



(** {1 HES} *)

(** {2 Equation} *)

(** Untyped version of {!Hflz.hes_rule} *)
type hes_rule = {
  var : string;
  args : string list;
  fix : Fixpoint.t;
  body : raw_hflz;
}

(** {3 Derived functions } *)

val equal_hes_rule : hes_rule -> hes_rule -> bool
val compare_hes_rule : hes_rule -> hes_rule -> int
val pp_hes_rule :
  Format.formatter ->
  hes_rule -> unit
val show_hes_rule : hes_rule -> string
val iter_hes_rule : hes_rule -> unit
val map_hes_rule : hes_rule -> hes_rule
val fold_hes_rule : 'a -> hes_rule -> 'a
val hes_rule_of_sexp : Sexplib0.Sexp.t -> hes_rule
val sexp_of_hes_rule : hes_rule -> Sexplib0.Sexp.t

(** {2 Datatype of HES} *)

(** Untyped version of {!Hflz.Sugar.hes}.
    Note that the goal fomula is not explicit.
    We consider the first element of the list as the goal formula.
*)
type hes = hes_rule list
(* TODO fix this discrepancy *)


(** {3 Derived functions } *)

val equal_hes : hes -> hes -> bool
val compare_hes : hes -> hes -> int
val pp_hes : Format.formatter -> hes -> unit
val show_hes : hes -> string
val iter_hes : 'a -> unit
val map_hes : 'a -> 'a
val fold_hes : 'a -> 'b -> 'a
val hes_of_sexp : Sexplib0.Sexp.t -> hes
val sexp_of_hes : hes -> Sexplib0.Sexp.t


(** {1 Typing} *)

module Typing :
  sig
  (** Type checker module *)

    val log_src : Logs.src
    module Log : Logs.LOG
    exception Error of string
    val error : string -> 'a

    (** Simple type extended with type variables *)
    type tyvar =
        TvRef of int * tyvar option ref
      | TvInt
      | TvBool
      | TvArrow of tyvar * tyvar

    val pp_tyvar : tyvar Print.t
    val pp_hum_tyvar : tyvar Print.t
    val show_tyvar : tyvar -> string

    (** Creates a fresh type variable *)
    val new_tyvar : unit -> tyvar


    exception Alias
    type occur_check_result = [ `Alias | `Ok ]
    val occur_check : tyvar option ref -> tyvar -> occur_check_result
    val unify : tyvar -> tyvar -> unit
    type id_env
    val pp_id_env : id_env Print.t
    type ty_env
    class add_annot :
      object
        val mutable ty_env : ty_env
        val mutable unbound_ints : id_env
        method add_ty_env : 'a Id.t -> tyvar -> unit
        method arith : id_env -> raw_hflz -> Arith.t
        method get_ty_env : ty_env
        method get_unbound_ints : id_env
        method hes : hes -> unit Hflz.Sugar.hes
        method hes_rule :
          id_env -> hes_rule -> unit Hflz.Sugar.hes_rule
        method term :
          id_env -> raw_hflz -> tyvar -> unit Hflz.Sugar.t
      end
    exception IntType
    class deref :
      ty_env ->
      object
        val ty_env : ty_env
        method arg_id :
          unit Type.arg Id.t ->
          Type.simple_ty Type.arg Id.t
        method arg_ty :
          string -> tyvar -> Type.simple_ty Type.arg
        method hes :
          unit Hflz.Sugar.hes ->
          Type.simple_ty Hflz.Sugar.hes
        method hes_rule :
          unit Hflz.Sugar.hes_rule ->
          Type.simple_ty Hflz.Sugar.hes_rule
        method id : unit Id.t -> Type.simple_ty Id.t
        method term :
          unit Hflz.Sugar.t -> Type.simple_ty Hflz.Sugar.t
        method ty : string -> tyvar -> Type.simple_ty
      end
    val to_typed :
      hes ->
      Type.simple_ty Hflz.Sugar.t *
      Type.simple_ty Hflz.Sugar.hes_rule list
  end

(** Type checks an untyped HES.
    [to_typed untyped_hes env] returns [(typed_hes, ty_env)],
    where [ty_env] is the type environment, which is a map from free variables to simple types.
*)
val to_typed :
  hes * (string * Formula.t list Type.ty) list ->
  Type.simple_ty Hflz.Sugar.hes * Type.simple_ty IdMap.t
