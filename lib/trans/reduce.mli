(** Module for reducing formulas *)

module Hflz__ :
  sig

    (** Beta-reduces redexes of a body formula.
        Predicate variables are not unfolded by this function
        (and thus additional redexes won't be introduced). *)
    val beta : 'a Hflz.t -> 'a Hflz.t

    (* TODO: document that it checks whether predicates are mutually recursivevly defined *)
    (** Inlines predicate variables *)
    val inline : Type.simple_ty Hflz.hes -> Type.simple_ty Hflz.hes
  end

module Hflz = Hflz__
