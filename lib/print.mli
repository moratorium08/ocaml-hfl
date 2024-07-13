(** Module for printing which extends [Stdlib.Format] *)

(** ['a Print.t] is simply a type for the formatter of type ['a]. *)
(** That is, ['a Print.t = 'a t = Format.formatter -> 'a -> unit ] *)
(** This module includes [Fmt] from the fmt library *)


include module type of Format
include module type of Fmt


(** Utility operators and functions *)

val ( ^^ ) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('f, 'b, 'c, 'e, 'g, 'h) format6 -> ('a, 'b, 'c, 'd, 'g, 'h) format6

val semicolon : unit t
val list_comma : 'a t -> 'a list t
val list_semi : 'a t -> 'a list t
val list_set : 'a t -> 'a list t

(** HFL specific formatter starts here *)
(** In the following, a function [foo_] is defined as [foo] with operator precendence *)
(* Do we realy want [foo_] to be exposed? *)

module Prec :
  sig
  (** Module for operator precedence *)

    type t = int
    val succ : int -> int
    val succ_if : bool -> int -> int
    val zero : int
    val arrow : int
    val abs : int
    val or_ : int
    val and_ : int
    val eq : int
    val add : int
    val mult : int
    val neg : int
    val app : int
    val of_op : Arith.op -> int
    val op_is_leftassoc : Arith.op -> bool
    val op_is_rightassoc : Arith.op -> bool
    val of_pred : 'a -> int
  end

type prec = Prec.t
type 'a t_with_prec = Prec.t -> 'a t
val ignore_prec : 'a t -> 'a t_with_prec
val show_paren :
  bool -> formatter -> ('a, formatter, unit) Base.format -> 'a

val void : Formula.Void.t t
val void_ : Formula.Void.t t_with_prec
val id : 'ty Id.t t
val id_ : [ `Int ] Id.t t_with_prec
val op : Arith.op t
val op_ : Arith.op t_with_prec
val gen_arith : 'avar t_with_prec -> 'avar Arith.gen_t t
val gen_arith_ : 'avar t_with_prec -> 'avar Arith.gen_t t_with_prec
val arith : Arith.t t
val arith_ : Prec.t -> Arith.t t
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
val argty_ : (Prec.t -> 'ty t) -> Prec.t -> 'ty Type.arg t
val ty : ?with_var:bool -> 'annot t -> 'annot Type.ty t
val ty_ : ?with_var:bool -> 'annot t -> Prec.t -> 'annot Type.ty t
val simple_ty : Type.simple_ty t
val simple_ty_ : Prec.t -> Type.simple_ty t
val simple_argty : Type.simple_ty Type.arg t
val simple_argty_ : Prec.t -> Type.simple_ty Type.arg t
val fixpoint : Fixpoint.t t
val hflz : (Prec.t -> 'ty t) -> 'ty Hflz.t t
val hflz_ : (Prec.t -> 'ty t) -> Prec.t -> 'ty Hflz.t t
val hflz_hes_rule : (Prec.t -> 'ty t) -> 'ty Hflz.hes_rule t
val hflz_hes : (Prec.t -> 'ty t) -> 'ty Hflz.hes t
