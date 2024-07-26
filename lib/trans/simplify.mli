(** Module for simplifying HFL formulas *)


(** Simplifies a HFL(Z) body formula by detecting "trivially true/false" subformulas.
    See {!Trans.Simplify} for details. *)
val hflz : 'a Hflz.t -> 'a Hflz.t

(** Simplifies an eqiaton of HES by detecting "trivially true/false" subformulas.
    See {!Trans.Simplify} for details. *)
val hflz_hes_rule : 'a Hflz.hes_rule -> 'a Hflz.hes_rule

(** Simplifies a HES by detecting "trivially true/false" subformulas.
    If [?inline=true], inlining is done before simplification.
    See {!Trans.Simplify} for details. *)
val hflz_hes : ?inline:bool -> Type.simple_ty Hflz.hes -> Type.simple_ty Hflz.hes

(** Returns [true] if the formuala can be trivially simplified to true *)
val is_true_def : ('a, 'b) Formula.gen_t -> bool

(** Returns [true] if the formuala can be trivially simplified to false *)
val is_false_def : ('a, 'b) Formula.gen_t -> bool

(** Simplifies a formula by detecting "trivially true/false" subformulas *)
val formula :
  ?is_true:(('bvar, 'avar) Formula.gen_t -> bool) ->
  ?is_false:(('bvar, 'avar) Formula.gen_t -> bool) ->
  ('bvar, 'avar) Formula.gen_t -> ('bvar, 'avar) Formula.gen_t
