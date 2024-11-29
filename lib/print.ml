open Base

include Stdlib.Format
include Fmt

let (^^) = Stdlib.(^^)

let semicolon = fun ppf () -> string ppf ";"

let list_comma format_x ppf xs =
    let sep ppf () = Fmt.pf ppf ",@," in
    Fmt.pf ppf "[@[%a@]]" Fmt.(list ~sep format_x) xs
let list_semi format_x ppf xs =
    let sep ppf () = Fmt.pf ppf ";@," in
    Fmt.pf ppf "[@[%a@]]" Fmt.(list ~sep format_x) xs
let list_set format_x ppf xs =
    let sep ppf () = Fmt.pf ppf ",@," in
    Fmt.pf ppf "{@[%a@]}" Fmt.(list ~sep format_x) xs

module Prec = struct
  type t = int
  let succ x = x + 1
  let succ_if b x = if b then x + 1 else x

  let zero  = 0
  let arrow = 1
  let abs   = 1
  let or_   = 2
  let and_  = 3
  let eq    = 4
  let add   = 6
  let mult  = 7
  let neg   = 9
  let app   = 10

  let of_op = function
    | Arith.Add -> add
    | Arith.Sub -> add
    | Arith.Mult -> mult
    | Arith.Div -> mult
    | Arith.Mod -> mult
  let op_is_leftassoc = function
    | Arith.Add -> true
    | Arith.Sub -> true
    | Arith.Mult -> true
    | Arith.Div -> true
    | Arith.Mod -> true
  let op_is_rightassoc = function
    | Arith.Add -> false
    | Arith.Sub -> false
    | Arith.Mult -> false
    | Arith.Div -> false
    | Arith.Mod -> false
  let of_pred = fun _ -> eq
end

type prec = Prec.t
type 'a t_with_prec = Prec.t -> 'a t

let ignore_prec orig _prec ppf x =
      orig ppf x

let show_paren b ppf fmt =
    if b
    then Fmt.pf ppf ("(" ^^ fmt ^^ ")")
    else Fmt.pf ppf fmt

let void _ v = Nothing.unreachable_code v
let void_ = ignore_prec void

let id ppf x = Fmt.pf ppf "%s" (Id.to_string x)
let id_ = ignore_prec id

(* Arith *)

let op = Arith.pp_op
let op_ = ignore_prec op

let rec gen_arith_ avar_ prec ppf =
  let open Arith in
  function
    | Int n -> Fmt.int ppf n
    | Var x -> avar_ prec ppf x
    | Op (Sub,[Int 0;a]) ->
        show_paren (prec > Prec.neg) ppf "-%a"
          (gen_arith_ avar_ Prec.(succ neg)) a
    | Op (op',[a1;a2]) ->
        let op_prec = Prec.of_op op' in
        let prec_l = Prec.(succ_if (not @@ op_is_leftassoc op') op_prec) in
        let prec_r = Prec.(succ_if (not @@ op_is_rightassoc op') op_prec) in
        show_paren (prec > op_prec) ppf "@[<1>%a@ %a@ %a@]"
          (gen_arith_ avar_ prec_l) a1
          op op'
          (gen_arith_ avar_ prec_r) a2
    | _ -> assert false
let gen_arith avar_ = gen_arith_ avar_ Prec.zero
let arith_ = gen_arith_ id_
let arith = arith_ Prec.zero

(* Formula *)

let pred ppf =
  let open Formula in
  function
  | Eq  -> Fmt.string ppf "="
  | Neq -> Fmt.string ppf "/="
  | Le  -> Fmt.string ppf "<="
  | Ge  -> Fmt.string ppf ">="
  | Lt  -> Fmt.string ppf "<"
  | Gt  -> Fmt.string ppf ">"
let pred_ = ignore_prec pred

let rec gen_formula_ bvar avar prec ppf (f : ('avar, 'bvar) Formula.gen_t) =
   match f with
    | Var x      -> bvar prec ppf x
    | Bool true  -> Fmt.string ppf "true"
    | Bool false -> Fmt.string ppf "false"
    | Or fs ->
        let sep ppf () = Fmt.pf ppf "@ || " in
        show_paren (prec > Prec.or_) ppf "@[<hv 0>%a@]"
          (list ~sep (gen_formula_ bvar avar Prec.or_)) fs
    | And fs ->
        let sep ppf () = Fmt.pf ppf "@ && " in
        show_paren (prec > Prec.and_) ppf "@[<hv 0>%a@]"
          (list ~sep (gen_formula_ bvar avar Prec.and_)) fs
    | Pred(pred',[f1;f2]) ->
        Fmt.pf ppf "@[<1>%a@ %a@ %a@]"
          (gen_arith_ avar prec) f1
          pred pred'
          (gen_arith_ avar prec) f2
    | Pred _ -> assert false
let gen_formula bvar avar ppf f = gen_formula_ bvar avar Prec.zero ppf f
let formula_ = gen_formula_ void_ id_
let formula = formula_ Prec.zero

(* Type *)

let argty_ format_ty_ prec ppf (arg : 'ty Type.arg) =
  match arg with
    | TyInt -> Fmt.string ppf "int"
    | TySigma sigma -> format_ty_ prec ppf sigma

let argty format_ty ppf (arg : 'ty Type.arg) =
  match arg with
    | TyInt -> Fmt.string ppf "int"
    | TySigma sigma -> format_ty ppf sigma

let rec ty_ ?(with_var=true) format_annot prec ppf (ty : 'annot Type.ty) =
  match ty with
      | TyBool annot ->
          Fmt.pf ppf "bool@[%a@]" format_annot annot
      | TyArrow (x, ret) ->
          if with_var then
            show_paren (prec > Prec.arrow) ppf "@[<1>%a:%a ->@ %a@]"
              id x
              (argty (ty_ ~with_var format_annot Prec.(succ arrow))) x.ty
              (ty_ ~with_var format_annot Prec.arrow) ret
          else
            show_paren (prec > Prec.arrow) ppf "@[<1>%a ->@ %a@]"
              (argty (ty_ ~with_var format_annot Prec.(succ arrow))) x.ty
              (ty_ ~with_var format_annot Prec.arrow) ret
let ty  ?(with_var=true) format_annot = ty_ ~with_var format_annot Prec.zero

let simple_ty_ = ty_ ~with_var:false Fmt.nop
let simple_ty = simple_ty_ Prec.zero
let simple_argty_ = argty_ simple_ty_
let simple_argty = simple_argty_ Prec.zero


 (* Fixpoint *)

let fixpoint ppf =
  let open Fixpoint in
  function
  | Least    -> Fmt.string ppf "μ"
  | Greatest -> Fmt.string ppf "ν"

(* Hflz *)

let rec hflz_ format_ty_ prec ppf (phi : 'ty Hflz.t)  =
  match phi with
    | Bool true -> Fmt.string ppf "true"
    | Bool false -> Fmt.string ppf "false"
    | Var x -> id ppf x
    | Or(phi1,phi2)  ->
        show_paren (prec > Prec.or_) ppf "@[<hv 0>%a@ || %a@]"
          (hflz_ format_ty_ Prec.or_) phi1
          (hflz_ format_ty_ Prec.or_) phi2
    | And (phi1,phi2)  ->
        show_paren (prec > Prec.and_) ppf "@[<hv 0>%a@ && %a@]"
          (hflz_ format_ty_ Prec.and_) phi1
          (hflz_ format_ty_ Prec.and_) phi2
    | Abs (x, psi) ->
        show_paren (prec > Prec.abs) ppf "@[<1>λ%a:%a.@,%a@]"
          id x
          (argty (format_ty_ Prec.(succ arrow))) x.ty
          (hflz_ format_ty_ Prec.abs) psi
    | Forall (x, psi) ->
        show_paren (prec > Prec.abs) ppf "@[<1>∀%a:%a.@,%a@]"
          id x
          (argty (format_ty_ Prec.(succ arrow))) x.ty
          (hflz_ format_ty_ Prec.abs) psi
    | Exists (x, psi) ->
        show_paren (prec > Prec.abs) ppf "@[<1>∃%a:%a.@,%a@]"
          id x
          (argty (format_ty_ Prec.(succ arrow))) x.ty
          (hflz_ format_ty_ Prec.abs) psi
    | App (psi1, psi2) ->
        show_paren (prec > Prec.app) ppf "@[<1>%a@ %a@]"
          (hflz_ format_ty_ Prec.app) psi1
          (hflz_ format_ty_ Prec.(succ app)) psi2
    | Arith a ->
        arith_ prec ppf a
    | Pred (pred, as') ->
        show_paren (prec > Prec.eq) ppf "%a"
          formula (Formula.Pred(pred, as'))
let hflz format_ty_ = hflz_ format_ty_ Prec.zero

let hflz_hes_rule format_ty_ ppf (rule : 'ty Hflz.hes_rule) =
    Fmt.pf ppf "@[<2>%s : %a =%a@ %a@]"
      (Id.to_string rule.var)
      (format_ty_ Prec.zero) rule.var.ty
      fixpoint rule.fix
      (hflz format_ty_) rule.body

let hflz_hes format_ty_ ppf hes =
  Fmt.pf ppf "@[<v>%a@ s.t.@ %a@]"
    (hflz format_ty_) (Hflz.top_formula_of hes)
    (Fmt.list (hflz_hes_rule format_ty_)) (Hflz.equations_of hes)
