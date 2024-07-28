(** Module for substitutions *)

(** Module [Foo] (e.g. [Hflz]) implements the substitution whose codomain is
    the set of objects represended by [Foo] (e.g. HFL(Z) body formulas). *)

(** Environment *)
type 'a env = 'a IdMap.t

module Id__ :
  sig
    (** Functions to substitute varaibles/identifiers *)

    (** Substitutes variables in arithmetic expressions with variables *)
    val arith :
      [ `Int ] Id.t env -> Arith.t -> Arith.t

    (** Substitutes variables in arithmetic formulas with variables *)
    val formula :
      [ `Int ] Id.t IdMap.t -> Formula.t -> Formula.t
  end

module Arith__ :
  sig
  (** Functions to substitute arithemetic expressions *)

    (* TODO these functions should use Arith.t env *)

    (** Substitutes variables in arithmetic expressions with arithmetic expressions *)
    val arith : 'ty Id.t -> Arith.t -> Arith.t -> Arith.t

    (** Substitutes variables in arithmetic formulas with arithmetic expressions *)
    val formula : 'ty Id.t -> Arith.t -> Formula.t -> Formula.t

    (** Same as {!arith}, but for generic expressions.
    The first argumet [eq : 'var -> 'var -> bool] is the equality function for ['var] *)
    val arith_ : ('var -> 'var -> bool) -> 'var -> 'var Arith.gen_t -> 'var Arith.gen_t -> 'var Arith.gen_t

    (** Same as {!formula}, but for generic formulas.
        The first argumet [eq : 'var -> 'var -> bool] is the equality function for ['var] *)
    val formula_ :
      ('var -> 'var -> bool) ->
      'var -> 'var Arith.gen_t ->
      ('bvar, 'var) Formula.gen_t -> ('bvar, 'var) Formula.gen_t

  end

module Hflz__ :
  sig
    (** Functions to substitute HFL(Z) body formulas *)

    (** Substitutes variables in arithmetic expressions with HFL(Z) body formula *)
    val arith : 'ty Hflz.t env -> Arith.t -> Arith.t

    (** Substitutes variables in HFL(Z) body formula with HFL(Z) body formula *)
    val hflz : 'ty Hflz.t env -> 'ty Hflz.t -> 'ty Hflz.t
  end

module Id = Id__
module Arith = Arith__
module Hflz = Hflz__
