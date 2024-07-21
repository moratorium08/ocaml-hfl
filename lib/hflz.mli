(** Module for HFL(Z) syntax trees *)


(** {1 Pseudo Formula} *)

module Sugar :
  sig
  (** Module for (pseudo) formulas that may contain negation *)
  (** These negations should be considered as macros which will later be expanded *)

    type 'ty t =
        Bool of bool
      | Var of 'ty Id.t
      | Or of 'ty t * 'ty t
      | And of 'ty t * 'ty t
      | Not of 'ty t
      | Abs of 'ty Type.arg Id.t * 'ty t
      | Forall of 'ty Type.arg Id.t * 'ty t
      | Exists of 'ty Type.arg Id.t * 'ty t
      | App of 'ty t * 'ty t
      | Arith of Arith.t
      | Pred of Formula.pred * Arith.t list

    (** Derived functions*)

    val equal : ('ty -> 'ty -> bool) -> 'ty t -> 'ty t -> bool
    val compare : ('ty -> 'ty -> int) -> 'ty t -> 'ty t -> int
    val pp :
      (Format.formatter -> 'ty -> unit) ->
      Format.formatter -> 'ty t -> unit
    val show : (Format.formatter -> 'ty -> unit) -> 'ty t -> string
    val iter : ('ty -> unit) -> 'ty t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val t_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty t
    val sexp_of_t : ('ty -> Sexplib0.Sexp.t) -> 'ty t -> Sexplib0.Sexp.t

    type 'ty hes_rule = {
      var : 'ty Id.t;
      body : 'ty t;
      fix : Fixpoint.t;
    }

    (** Derived functions *)

    val equal_hes_rule : ('ty -> 'ty -> bool) -> 'ty hes_rule -> 'ty hes_rule -> bool
    val compare_hes_rule :
      ('ty -> 'ty -> int) ->
      'ty hes_rule -> 'ty hes_rule -> int
    val pp_hes_rule :
      (Format.formatter ->
       'ty -> unit) ->
      Format.formatter ->
      'ty hes_rule -> unit
    val show_hes_rule :
      (Format.formatter ->
       'ty -> unit) ->
      'ty hes_rule -> string
    val iter_hes_rule : ('ty -> unit) -> 'ty hes_rule -> unit
    val map_hes_rule : ('ty -> 'a) -> 'ty hes_rule -> 'a hes_rule
    val fold_hes_rule : ('a -> 'b -> 'a) -> 'a -> 'b hes_rule -> 'a
    val hes_rule_of_sexp :
      (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty hes_rule
    val sexp_of_hes_rule :
      ('ty -> Sexplib0.Sexp.t) -> 'ty hes_rule -> Sexplib0.Sexp.t
    type 'ty hes = 'ty t * 'ty hes_rule list
    val equal_hes :
      ('ty -> 'ty -> bool) ->
      'ty hes -> 'ty hes -> bool
    val compare_hes :
      ('ty -> 'ty -> int) ->
      'ty hes -> 'ty hes -> int
    val pp_hes :
      (Format.formatter ->
       'ty -> unit) ->
      Format.formatter ->
      'ty hes -> unit
    val show_hes :
      (Format.formatter ->
       'ty -> unit) ->
      'ty hes -> string
    val iter_hes : ('ty -> unit) -> 'ty t * 'ty hes_rule list -> unit
    val map_hes :
      ('a -> 'b) -> 'a t * 'a hes_rule list -> 'b t * 'b hes_rule list
    val fold_hes : ('a -> 'b -> 'a) -> 'a -> 'b t * 'b hes_rule list -> 'a
    val hes_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty hes
    val sexp_of_hes : ('ty -> Sexplib0.Sexp.t) -> 'ty hes -> Sexplib0.Sexp.t

    (** Costructors *)

    val mk_var : 'a Id.t -> 'a t
    val mk_abs : 'a Type.arg Id.t -> 'a t -> 'a t
    val mk_abss : 'a Type.arg Id.t list -> 'a t -> 'a t


    val decompose_abs : 'a t -> 'a Type.arg Id.t list * 'a t
  end

(** {1 Body Formula} *)

 (** HFL(Z) body formuala.
 The variables appearing in the syntax tree caree type information.
 In most cases, the parameter ['ty] will be instantiated as {!Type.simple_ty} *)
type 'ty t =
    Bool of bool
  | Var of 'ty Id.t
  | Or of 'ty t * 'ty t
  | And of 'ty t * 'ty t
  | Abs of 'ty Type.arg Id.t * 'ty t
  | Forall of 'ty Type.arg Id.t * 'ty t
  | Exists of 'ty Type.arg Id.t * 'ty t
  | App of 'ty t * 'ty t
  | Arith of Arith.t
  | Pred of Formula.pred * Arith.t list

(** {2 Derived functions} *)

val equal : ('ty -> 'ty -> bool) -> 'ty t -> 'ty t -> bool
val compare : ('ty -> 'ty -> int) -> 'ty t -> 'ty t -> int
val pp : (Format.formatter -> 'ty -> unit) -> Format.formatter -> 'ty t -> unit
val show : (Format.formatter -> 'ty -> unit) -> 'ty t -> string
val iter : ('ty -> unit) -> 'ty t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val t_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty t
val sexp_of_t : ('ty -> Sexplib0.Sexp.t) -> 'ty t -> Sexplib0.Sexp.t

(** {1 HES} *)

(** We use an equational presentation for HFL(Z) formulas.
    Each equation is of the form {m X : \tau =_{\alpha} \varphi},
    where {m \alpha} is either {m \mu} or {m \nu}.
    Hierarchial equational system (HES) is a finite set of these equations.
*)

(** {2 Equation}*)

(** Equation of HES. That is, {m X : \tau =_{\alpha} \varphi}. [var] is {m X : \tau}, [body] is {m \varphi} and [fix] is {m \alpha}. *)
type 'ty hes_rule = {
  var : 'ty Id.t;
  body : 'ty t;
  fix : Fixpoint.t;
}

(** {3 Derived functions} *)

val equal_hes_rule :
  ('ty -> 'ty -> bool) ->
  'ty hes_rule -> 'ty hes_rule -> bool
val compare_hes_rule :
  ('ty -> 'ty -> int) ->
  'ty hes_rule -> 'ty hes_rule -> int
val pp_hes_rule :
  (Format.formatter -> 'ty -> unit) ->
  Format.formatter ->
  'ty hes_rule -> unit
val show_hes_rule :
  (Format.formatter -> 'ty -> unit) ->
  'ty hes_rule -> string
val iter_hes_rule : ('ty -> unit) -> 'ty hes_rule -> unit
val map_hes_rule : ('ty -> 'a) -> 'ty hes_rule -> 'a hes_rule
val fold_hes_rule : ('a -> 'b -> 'a) -> 'a -> 'b hes_rule -> 'a
val hes_rule_of_sexp :
  (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty hes_rule
val sexp_of_hes_rule :
  ('ty -> Sexplib0.Sexp.t) -> 'ty hes_rule -> Sexplib0.Sexp.t

(** {3 Non-derived functions} *)

val lookup_rule : 'ty Id.t -> 'ty hes_rule list -> 'ty hes_rule


(** {2 Datatype of HES} *)

(** HES which is a pair of a goal formula and equations *)
type 'ty hes = 'ty t * 'ty hes_rule list

(** {3 Derived functions} *)

val equal_hes : ('ty -> 'ty -> bool) -> 'ty hes -> 'ty hes -> bool
val compare_hes : ('ty -> 'ty -> int) -> 'ty hes -> 'ty hes -> int
val pp_hes :
  (Format.formatter -> 'ty -> unit) ->
  Format.formatter -> 'ty hes -> unit
val show_hes : (Format.formatter -> 'ty -> unit) -> 'ty hes -> string
val iter_hes : ('ty -> unit) -> 'ty t * 'ty hes_rule list -> unit
val map_hes :
  ('a -> 'b) -> 'a t * 'a hes_rule list -> 'b t * 'b hes_rule list
val fold_hes : ('a -> 'b -> 'a) -> 'a -> 'b t * 'b hes_rule list -> 'a
val hes_of_sexp : (Sexplib0.Sexp.t -> 'ty) -> Sexplib0.Sexp.t -> 'ty hes
val sexp_of_hes : ('ty -> Sexplib0.Sexp.t) -> 'ty hes -> Sexplib0.Sexp.t

(** {1 Methods} *)

(** {2 Constructors} *)

val mk_bool : bool -> 'ty t
val mk_var : 'ty Id.t -> 'ty t

(** [mk_ands [e1; ... en ] = e1 /\ ... /\ en ] *)
val mk_ands : 'ty t list -> 'ty t

(** [mk_ors [e1; ... en ] = e1 \/ ... \/ en ] *)
val mk_ors : 'ty t list -> 'ty t
val mk_pred : Formula.pred -> Arith.t -> Arith.t -> 'ty t
val mk_arith : Arith.t -> 'ty t
val mk_app : 'ty t -> 'ty t -> 'ty t

(** [mk_apps e0 [e1; ... ; en] = e0 e1 ... en ] *)
val mk_apps : 'ty t -> 'ty t list -> 'ty t

val mk_abs : 'ty Type.arg Id.t -> 'ty t -> 'ty t

(** [mk_abss [x1; ...; xn] e = \x1 ... xn . e ] *)
val mk_abss : 'ty Type.arg Id.t list -> 'ty t -> 'ty t

(** {2 Decomposers} *)

val decompose_abs : 'ty t -> 'ty Type.arg Id.t List.t * 'ty t
val decompose_app : 'ty t -> 'ty t * 'ty t list


(** {2 Others} *)

(** Desugar "not" from a formula.
    This function may fail and raise an error because not all negations can be represented as a macro *)
val desugar_formula : 'ty Sugar.t -> 'ty t

(** Desugar "not" from a HES.
    This function may fail and raise an error because not all negations can be represented as a macro *)
val desugar : 'ty Sugar.hes -> 'ty hes

(** Returns the set of free variables *)
val fvs : 'ty t -> IdSet.t

(** Returns the set of free predicate variables *)
val fpreds : 'ty t -> IdSet.t
