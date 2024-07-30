(** Module for printing which extends [Stdlib.Format] *)

(** ['a Print.t] is simply a type for the formatter of type ['a].
    That is, ['a Print.t = 'a Fmt.t = Format.formatter -> 'a -> unit ] *)
(** This module includes [Fmt] from the fmt library *)

(** {1 [Stdlib.Format] and [Fmt]} *)

include module type of Format
include module type of Fmt


(** {2 Additional utility operators and functions} *)

val ( ^^ ) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('f, 'b, 'c, 'e, 'g, 'h) format6 -> ('a, 'b, 'c, 'd, 'g, 'h) format6

val semicolon : unit t
val list_comma : 'a t -> 'a list t
val list_semi : 'a t -> 'a list t
val list_set : 'a t -> 'a list t


(** {1 Formatters for HFL} *)

(** In the following, a function [foo_] is defined as [foo] with operator precendence *)
(* Do we realy want [foo_] to be exposed? *)

(** {2 Operator Precendece}*)

module Prec :
  sig
  (** Module for operator precedence *)

    type t
    val succ : t -> t
    val succ_if : bool -> t -> t
    val zero : t
    val arrow : t
    val abs : t
    val or_ : t
    val and_ : t
    val eq : t
    val add : t
    val mult : t
    val neg : t
    val app : t
    val of_op : Arith.op -> t
    val op_is_leftassoc : Arith.op -> bool
    val op_is_rightassoc : Arith.op -> bool
    val of_pred : 'a -> t
  end

type prec = Prec.t
type 'a t_with_prec = prec -> 'a t
val ignore_prec : 'a t -> 'a t_with_prec


(** {2 Formatter} *)

val show_paren :
  bool -> formatter -> ('a, formatter, unit) Base.format -> 'a

val void : Base.Nothing.t t
val void_ : Base.Nothing.t t_with_prec
val id : 'ty Id.t t
val id_ : [ `Int ] Id.t t_with_prec
val op : Arith.op t
val op_ : Arith.op t_with_prec
val gen_arith : 'avar t_with_prec -> 'avar Arith.gen_t t
val gen_arith_ : 'avar t_with_prec -> 'avar Arith.gen_t t_with_prec
val arith : Arith.t t
val arith_ : prec -> Arith.t t
val pred : Formula.pred t
val pred_ : Formula.pred t_with_prec
val gen_formula :
  'bvar t_with_prec ->
  'avar t_with_prec -> ('bvar, 'avar) Formula.gen_t t
val gen_formula_ :
  'bvar t_with_prec ->
  'avar t_with_prec -> ('bvar, 'avar) Formula.gen_t t_with_prec
val formula : Formula.t t
val formula_ : Formula.t t_with_prec
val argty : 'ty t -> 'ty Type.arg t
val argty_ : (prec -> 'ty t) -> prec -> 'ty Type.arg t
val ty : ?with_var:bool -> 'annot t -> 'annot Type.ty t
val ty_ : ?with_var:bool -> 'annot t -> prec -> 'annot Type.ty t
val simple_ty : Type.simple_ty t
val simple_ty_ : prec -> Type.simple_ty t
val simple_argty : Type.simple_ty Type.arg t
val simple_argty_ : prec -> Type.simple_ty Type.arg t
val fixpoint : Fixpoint.t t
val hflz : (prec -> 'ty t) -> 'ty Hflz.t t
val hflz_ : (prec -> 'ty t) -> prec -> 'ty Hflz.t t
val hflz_hes_rule : (prec -> 'ty t) -> 'ty Hflz.hes_rule t
val hflz_hes : (prec -> 'ty t) -> 'ty Hflz.hes t
