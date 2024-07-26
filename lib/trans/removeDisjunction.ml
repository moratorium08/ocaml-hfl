open Base
 (* remove disjunction translator *)
(**
  A is valid <=> [A]false is valid
  [true]k = true
  [false]k = k
  [e1<e2]k = e1<e2 ∨ k  (i.e. e1>=e2 => k)
  [A∨B]k = [A]([B]k)
  [A∧B]k = [A]k ∧ [B]k
  [νX.A] = νX.[A]
  [∀x.A]k = ∀x. [A]k
  [A B] = [A] [B]
  [A e] = [A] e
  [λX.A] = λX.[A]
  [X] = X
**)

let gen_cont_var () =
  (*let t = Type.TyArrow(tmp_arg, Type.TyBool()) in*)
  let t = Type.TyBool () in
  Id.gen t

(* check if the translation is required.*)
let rec check_body in_disj = function
  | Hflz.Bool _
  | Hflz.Pred _
  | Hflz.Arith _ -> false
  | Hflz.Or(x, y) ->
    check_body true x || check_body true y
  | Hflz.And(x, y) ->
    check_body in_disj x || check_body in_disj y
  | Hflz.Abs(_, y) -> check_body in_disj y
  | Hflz.Forall(_, y) -> check_body in_disj y
  (* TODO better error reporting  *)
  | Hflz.Exists(_) -> failwith "remove disjunction is only supported for nuHFL"
  | Hflz.App _ when in_disj -> true
  | Hflz.App (x, y) -> check_body in_disj x || check_body in_disj y
  | Hflz.Var x when in_disj -> begin
      match x.ty with
      | Type.TyBool _ -> true
      | _ -> false
    end
  | Hflz.Var _ -> false

let check_hes_rule (rule:Type.simple_ty Hflz.hes_rule)
  = check_body false rule.body

let rec check hes =
  check_body false (Hflz.top_formula_of hes)
  || List.exists (Hflz.equations_of hes) check_hes_rule

let tmp_arg =
  Type.TyBool ()
  |> Id.gen
  |> Type.lift_arg

let prop2prop = Type.TyArrow(tmp_arg, Type.TyBool())

let rec lift_ty (t:Type.simple_ty) = (match t with
    | Type.TyBool _ -> prop2prop
    | Type.TyArrow(x, y) ->
      let x' = lift_ty_arg x in
      let y' = lift_ty y in
      Type.TyArrow(x', y'))
and lift_ty_arg (t:Type.simple_argty Id.t) = match t.ty with
    | Type.TyInt -> t
    | Type.TySigma x ->
      {t with ty=Type.TySigma (lift_ty x)}

let rec translate_body body =
  let open Hflz in
  let ret_k = gen_cont_var () in
  let k = Type.lift_arg ret_k in
  let ret_k_var = Var ret_k in
  match body with
  | Bool true -> Abs(k, Bool true)
  | Bool false -> Abs(k, ret_k_var)
  | Pred(p, l) -> Abs(k, Or(Pred(p, l), ret_k_var))
  | Or(x, y) ->
    let x' = translate_body x in
    let y' = translate_body y in
    Abs(k, App(x', App(y', ret_k_var)))
  | And(x, y) ->
    let x' = translate_body x in
    let y' = translate_body y in
    Abs(k, And(App(x', ret_k_var), App(y', ret_k_var)))
  | Forall(x, y) ->
    let y' = translate_body y in
    Abs(k, Forall(x, App(y', ret_k_var)))
  (* TODO better error reporting  *)
  | Hflz.Exists(_) -> failwith "remove disjunction is only supported for nuHFL"
  | App(x, y) ->
    let x' = translate_body x in
    let y' = translate_body y in
    App(x', y')
  | Arith x -> Arith x
  | Abs(id, y) ->
    let y' = translate_body y in
    let id = lift_ty_arg id in
    Abs(id, y')
  | Var x -> Var x

let translate_hes_rule (rule : Type.simple_ty Hflz.hes_rule) =
  {rule with body=translate_body rule.body}

let translate_top top =
  Hflz.App(translate_body top, Bool false)

let f hes =
  if check hes then
    (* TODO Switch to Stdio or use the Logger? *)
    let top_formula = translate_top (Hflz.top_formula_of hes) in
    let rules = List.map ~f:translate_hes_rule (Hflz.equations_of hes) in
    let _ = Stdlib.Printf.printf "[REMOVE_DISJUNCTION]\n" in
    Hflz.mk_hes top_formula rules
  else
    (Stdlib.Printf.printf "[NO_DISJUNCTION]\n"; hes)
