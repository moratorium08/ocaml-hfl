open Base
open Id
open Type

(* Notを追加したHFLzのsyntax sugar *)
module Sugar = struct
  type 'ty t =
    | Bool   of bool
    | Var    of 'ty Id.t
    | Or     of 'ty t * 'ty t
    | And    of 'ty t * 'ty t
    | Not    of 'ty t
    | Abs    of 'ty arg Id.t * 'ty t
    | Forall of 'ty arg Id.t * 'ty t
    | Exists of 'ty arg Id.t * 'ty t
    | App    of 'ty t * 'ty t
    | Arith  of Arith.t
    | Pred   of Formula.pred * Arith.t list
    [@@deriving eq,ord,show,iter,map,fold,sexp]

  type 'ty hes_rule =
    { var  : 'ty Id.t
    ; body : 'ty t
    ; fix  : Fixpoint.t
    }
    [@@deriving eq,ord,show,iter,map,fold,sexp]

  type 'ty hes = 'ty t * 'ty hes_rule list
    [@@deriving eq,ord,show,iter,map,fold,sexp]

  let mk_var x : 'a t = Var x
  let mk_abs x t = Abs(x, t)
  let mk_abss xs t = List.fold_right xs ~init:t ~f:mk_abs
  let mk_hes top rules = top, rules

  let decompose_abs =
    let rec go acc phi = match phi with
      | Abs(x, phi) -> go (x::acc) phi
      | _ -> (List.rev acc, phi)
    in fun phi -> go [] phi
  let top_formula_of hes = fst hes
  let equations_of hes = snd hes
end

type 'ty t =
  | Bool   of bool
  | Var    of 'ty Id.t
  | Or     of 'ty t * 'ty t
  | And    of 'ty t * 'ty t
  | Abs    of 'ty arg Id.t * 'ty t
  | Forall of 'ty arg Id.t * 'ty t
  | Exists of 'ty arg Id.t * 'ty t
  | App    of 'ty t * 'ty t
  (* constructers only for hflz *)
  | Arith  of Arith.t
  | Pred   of Formula.pred * Arith.t list
  [@@deriving eq,ord,show,iter,map,fold,sexp]

exception CannotNegate
(* 全体を一度にnegateすると単純なやり方でよい。 *)
let negate_formula (formula : 'ty t) =
  let is_negation_of f1 f2 =
    let rec neg (f : 'ty t) : 'ty t = match f with
      | Bool b -> Bool (not b)
      | Or  (f1, f2) -> And (neg f1, neg f2)
      | And (f1, f2) -> Or  (neg f1, neg f2)
      | Forall (x, f) -> Exists (x, neg f)
      | Exists (x, f) -> Forall (x, neg f)
      | Pred (p, args) -> Pred (Formula.negate_pred p, args)
      | Arith _ | Var _ | Abs _ | App _ -> raise CannotNegate in
    try
      (** The equality function for 'ty is irrelevant so we always return false*)
      equal (fun _ _ -> false) (neg f1)  f2
    with CannotNegate -> false
  in
  let rec go formula = match formula with
    | Bool b -> Bool (not b)
    | Var x  -> Var x
    | And (Or (f1, f2), Or(f3, f4)) when is_negation_of f1 f3 ->
      (* ifのとき *)
      (* !((p \/ q) /\ (!p \/ r))  =  (!p /\ !q) \/ (p /\ !r)  =
         (!p => !q) /\ (p => !r)  =  ((p \/ !q) /\ (!p \/ !r)) *)
      (* print_endline "NEGATE IF!!!"; *)
      And (Or (f1, go f2), Or(f3, go f4))
    | Or  (f1, f2) -> And (go f1, go f2)
    | And (f1, f2) -> Or  (go f1, go f2)
    | Abs (x, f1)  -> Abs (x, go f1)
    | App (f1, f2) -> App (go f1, go f2)
    | Forall (x, f) -> Exists (x, go f)
    | Exists (x, f) -> Forall (x, go f)
    | Arith (arith) -> Arith (arith)
    | Pred (p, args) -> Pred (Formula.negate_pred p, args) in
  go formula

type 'ty hes_rule =
  { var  : 'ty Id.t
  ; body : 'ty t
  ; fix  : Fixpoint.t
  }
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let negate_rule {var; body; fix} =
  {var; body = negate_formula body; fix = Fixpoint.flip_fixpoint fix}

let lookup_rule f hes =
  List.find_exn hes ~f:(fun r -> Id.eq r.var f)

type 'ty hes = 'ty t * 'ty hes_rule list
    [@@deriving eq,ord,show,iter,map,fold,sexp]

 (* Construction *)
let mk_bool b = Bool b

let mk_var x = Var x

let mk_ands = function
  | [] -> Bool true
  | x::xs -> List.fold_left xs ~init:x ~f:(fun a b -> And(a,b))

let mk_ors = function
  | [] -> Bool false
  | x::xs -> List.fold_left xs ~init:x ~f:(fun a b -> Or(a,b))

let mk_pred pred a1 a2 = Pred(pred, [a1;a2])

let mk_arith a = Arith a

let mk_app t1 t2 = App(t1,t2)
let mk_apps t ts = List.fold_left ts ~init:t ~f:mk_app

let mk_abs x t = Abs(x, t)
let mk_abss xs t = List.fold_right xs ~init:t ~f:mk_abs

let mk_hes f fs = (f, fs)

(* Decomposition *)
let decompose_abs =
  let rec go acc phi = match phi with
    | Abs(x,phi) -> go (x::acc) phi
    | _ -> (List.rev acc, phi)
  in fun phi -> go [] phi

let decompose_app =
  let rec go phi acc = match phi with
    | App(phi,x) -> go phi (x::acc)
    | _ -> (phi, acc)
  in
  fun phi -> go phi []

let top_formula_of hes = fst hes
let equations_of hes = snd hes

let desugar_formula (formula : 'a Sugar.t) : 'a t =
  let rec neg (f : 'a Sugar.t) : 'a t = match f with
    | Bool b -> Bool (not b)
    | Or  (f1, f2) -> And (neg f1, neg f2)
    | And (f1, f2) -> Or  (neg f1, neg f2)
    | Forall (x, f) -> Exists (x, neg f)
    | Exists (x, f) -> Forall (x, neg f)
    | Pred (p, args) -> Pred (Formula.negate_pred p, args)
    | Arith _-> failwith "(negate_subformula) cannot negate Arith"
    | Var _  -> failwith "(negate_subformula) cannot negate Var"
    | Abs _  -> failwith "(negate_subformula) cannot negate Abs"
    | App _  -> failwith "(negate_subformula) cannot negate App"
    | Not f  -> thr f
  and thr (f : 'a Sugar.t) : 'a t = match f with
    | Var x  -> Var x
    | Bool b -> Bool b
    | Or  (phi1, phi2) -> Or  (thr phi1, thr phi2)
    | And (phi1, phi2) -> And (thr phi1, thr phi2)
    | App (phi1, phi2) -> App (thr phi1, thr phi2)
    | Abs (x, phi1)    -> Abs (x, thr phi1)
    | Forall (x, phi1) -> Forall (x, thr phi1)
    | Exists (x, phi1) -> Exists (x, thr phi1)
    | Arith a          -> Arith a
    | Pred (x, as')    -> Pred (x, as')
    | Not phi1         -> neg phi1 in
  thr formula

let desugar ((entry, rules) : 'a Sugar.hes) : 'a hes =
  desugar_formula entry,
  List.map ~f:(fun { var; body; fix } -> { var; fix; body = desugar_formula body }) rules

let dualize_hes  hes =
  let top_formula = negate_formula @@ top_formula_of hes  in
  let equations = List.map ~f:(fun rule -> negate_rule rule) @@ equations_of hes in
  mk_hes top_formula equations

let rec fvs = function
  | Var x          -> IdSet.singleton x
  | Bool _         -> IdSet.empty
  | Or (phi1,phi2) -> IdSet.union (fvs phi1) (fvs phi2)
  | And(phi1,phi2) -> IdSet.union (fvs phi1) (fvs phi2)
  | App(phi1,phi2) -> IdSet.union (fvs phi1) (fvs phi2)
  | Abs(x,phi)     -> IdSet.remove (fvs phi) x
  | Forall (x,phi) -> IdSet.remove (fvs phi) x
  | Exists (x,phi) -> IdSet.remove (fvs phi) x
  | Arith a        -> IdSet.of_list @@ List.map ~f:Id.remove_ty @@ Arith.fvs a
  | Pred (_,as')   -> IdSet.union_list @@ List.map as' ~f:begin fun a ->
                        IdSet.of_list @@ List.map ~f:Id.remove_ty @@ Arith.fvs a
                      end

let fpreds formula =
  fvs formula
  |> IdSet.filter ~f:begin fun x -> (* filter nonterminals *)
    let c = String.get x.Id.name 0 in
    Char.equal c @@  Char.uppercase c (* XXX ad hoc *)
  end
