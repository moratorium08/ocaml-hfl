open Base
open Type
open TransUtil

module S = TransUtil.ModuleWrapper

let hflz : 'a Hflz.t -> 'a Hflz.t =
  let rec is_trivially_true : 'a Hflz.t -> bool =
    fun phi -> match phi with
      | Bool b -> b
      | Or (phi1,phi2) -> is_trivially_true phi1 || is_trivially_true phi2
      | And(phi1,phi2) -> is_trivially_true phi1 && is_trivially_true phi2
      | _ -> false
  in
  let rec is_trivially_false : 'a Hflz.t -> bool =
    fun phi -> match phi with
      | Bool b -> not b
      | And(phi1,phi2) -> is_trivially_false phi1 || is_trivially_false phi2
      | Or (phi1,phi2) -> is_trivially_false phi1 && is_trivially_false phi2
      | _ -> false
  in
  let rec go phi =
    match Reduce.Hflz.beta phi with
      | And(phi1, phi2) ->
        let phi1 = go phi1 in
        let phi2 = go phi2 in
        let phis = List.filter ~f:(fun x -> not (is_trivially_true x)) [phi1;phi2] in
        Hflz.mk_ands phis
      | Or (phi1, phi2) ->
        let phi1 = go phi1 in
        let phi2 = go phi2 in
        let phis = List.filter ~f:(fun x -> not (is_trivially_false x)) [phi1;phi2] in
        Hflz.mk_ors phis
      | Abs(x,phi)     -> Abs(x, go phi)
      | App(phi1,phi2) -> App(go phi1, go phi2)
      | phi -> phi
  in go
let hflz_hes_rule : 'a Hflz.hes_rule -> 'a Hflz.hes_rule =
  fun rule -> { rule with body = hflz rule.body }
let hflz_hes : ?inline:bool -> simple_ty Hflz.hes -> simple_ty Hflz.hes =
  fun ?(inline=true) hes ->
  let hes =
    hes
    |> begin
      if inline
      then Reduce.Hflz.inline
      else (fun x -> x)
    end
  in
  let main = Hflz.top_formula_of hes in
  let rules = List.map ~f:hflz_hes_rule (Hflz.equations_of hes) in
  Hflz.mk_hes main rules

let rec is_true_def =
  fun phi -> match phi with
    | Formula.Bool b -> b
    | Formula.And phis -> List.for_all ~f:is_true_def phis
    | Formula.Or  phis -> List.exists  ~f:is_true_def phis
    | _ -> false
let rec is_false_def =
  fun phi -> match phi with
    | Formula.Bool b -> not b
    | Formula.And phis -> List.exists  ~f:is_false_def phis
    | Formula.Or  phis -> List.for_all ~f:is_false_def phis
    | _ -> false

let rec formula
  : 'bvar 'avar
  . ?is_true:(('bvar, 'avar) Formula.gen_t -> bool)
    -> ?is_false:(('bvar, 'avar) Formula.gen_t -> bool)
    -> ('bvar, 'avar) Formula.gen_t
    -> ('bvar, 'avar) Formula.gen_t =
  fun ?(is_true=is_true_def) ?(is_false=is_false_def) -> function
    | Formula.And phis ->
      let phis = List.map ~f:formula phis in
      let phis = List.filter ~f:(fun x -> not (is_true x)) phis in
      begin if List.exists ~f:is_false phis then
          Bool false
        else match phis with
          | []    -> Bool true
          | [phi] -> phi
          | _     -> And phis
      end
    | Formula.Or phis ->
      let phis = List.map ~f:formula phis in
      let phis = List.filter ~f:(fun x -> not (is_false x)) phis in
      begin if List.exists ~f:is_true phis then
          Bool true
        else match phis with
          | []    -> Bool false
          | [phi] -> phi
          | _     -> Or phis
      end
    | phi -> phi
