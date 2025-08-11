(** Module for alpha-renaming. Replaces the bound variables with {b fresh} names *)

(** alpha-conversion for hflz formulas *)
val hflz : 'ty Hflz.t -> 'ty Hflz.t

(** a variant of {!hflz} for open terms. [hflz' env phi] uses [env] to rename the free variables *)
val hflz' : 'ty Type.arg Id.t IdMap.t -> 'ty Hflz.t -> 'ty Hflz.t

(** alpha-conversion for an equation *)
val hes_rule : 'ty Hflz.hes_rule -> 'ty Hflz.hes_rule

(** alpha-conversion for a hes *)
val hes  : 'ty Hflz.hes -> 'ty Hflz.hes
