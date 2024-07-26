(** Module for removing disjunctions from a nuHFL(Z) formula by lifting the order *)

(** The translation {m \llbracket \varphi \rrbracket } translates an order n
    formula {m \varphi} to an order n+1 formula that doesn't contain any disjunctions.
    The translation preserves and reflects the validity: {m \models \varphi} iff {m \models \llbracket \varphi \rrbracket}.
    Removing disjunctions may improve the efficiency of an automatic nuHFL solver *)
(** The translation is inductively defined as follows: *)
(**  {math
     \begin{align*}
     \llbracket \mathbf{true} \rrbracket \; k &= \mathbf{true} \\
     \llbracket \mathbf{false} \rrbracket \; k &= k \\
     \llbracket e1<e2 \rrbracket \; k &= e1<e2 \lor k \\
     \llbracket \varphi_1 \lor \varphi \rrbracket \; k &= \llbracket \varphi_1 \rrbracket \; (\llbracket \varphi_2 \rrbracket \; k) \\
     \llbracket \varphi_1 \land \varphi \rrbracket \; k &= \llbracket \varphi_1 \rrbracket  \; k \land  \llbracket \varphi_2 \rrbracket \; k \\
     \llbracket \nu X. \varphi \rrbracket &= \nu X . \llbracket \varphi \rrbracket \\
     \llbracket \forall x. \varphi \rrbracket &= \forall x. \llbracket \varphi \rrbracket \\
     \llbracket \lambda x. \varphi \rrbracket &= \lambda x. \llbracket \varphi \rrbracket \\
     \llbracket \nu X. \varphi \rrbracket &= \nu X . \llbracket \varphi \rrbracket \\
     \llbracket \varphi_1 \; \varphi_2 \rrbracket &= \llbracket \varphi_1 \rrbracket \; \llbracket \varphi_2 \rrbracket \\
     \llbracket \varphi \; e \rrbracket &= \llbracket \varphi \rrbracket \; e \\
     \llbracket X \rrbracket &= X
     \end{align*}
     }
*)

(** @see <https://doi.org/10.1007/978-3-031-19135-0_8> for the details of the translation. *)

(** Checks whether a HES equation needs to be translated. *)
val check_body : bool -> 'a Type.ty Hflz.t -> bool

(** Checks whether a HES equation needs to be translated. *)
val check_hes_rule : Type.simple_ty Hflz.hes_rule -> bool

(** Checks whether a nuHFL(Z) formula needs to be translated. *)
val check : Type.simple_ty Hflz.hes -> bool

(** Removes disjunction from the body formula.
    Use this with care since the tranlsation is correct only when the entire
    formula is translated. *)
val translate_body : Type.simple_ty Hflz.t -> Type.simple_ty Hflz.t

(** Removes disjunction from a single HES equation.
    Use this with care since the tranlsation is correct only when the entire
    formula is translated. *)
val translate_hes_rule :
  Type.simple_ty Hflz.hes_rule -> Type.simple_ty Hflz.hes_rule

(** Translates the top formula by removing disjuntions, and then, applying it to false.
    Use this with care since the tranlsation is correct only when the entire
    formula is translated. *)
val translate_top : Type.simple_ty Hflz.t -> Type.simple_ty Hflz.t

(** Checks whether the transformation is needed, and then removes disjunctions if necessary *)
val f : Type.simple_ty Hflz.hes -> Type.simple_ty Hflz.hes
