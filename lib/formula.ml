open Base
open Id

type pred =
  | Eq
  | Neq
  | Le
  | Ge
  | Lt
  | Gt
  [@@deriving eq,ord,show,iter,map,fold,sexp]

(* formula parametrized by variable type and arith type *)
type ('bvar, 'avar) gen_t =
  | Bool of bool
  | Var  of 'bvar
  | Or   of ('bvar, 'avar) gen_t list
  | And  of ('bvar, 'avar) gen_t list
  | Pred of pred * 'avar Arith.gen_t list
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let negate_pred = function
  | Eq  -> Neq
  | Neq -> Eq
  | Le  -> Gt
  | Gt  -> Le
  | Lt  -> Ge
  | Ge  -> Lt

(* TODO this should be hidden *)
module Void = struct
  type t = Void of { absurd : 'a. 'a }
  let absurd (Void void) = void.absurd
  let equal v _     = absurd v
  let compare v _   = absurd v
  let pp _ v        = absurd v
  let t_of_sexp _   = failwith "void_of_sexp"
  let sexp_of_t v   = absurd v
end

(* type t = ((string * [`Pos|`Neg]), [`Int] Id.t) gen_t *)
type t = (Void.t, [`Int] Id.t) gen_t
  [@@deriving eq,ord,show,iter,map,fold,sexp]
let hash (x : t) : int = Sexp.hash (sexp_of_t x)

let mk_bool b = Bool b

let mk_var x = Var x

let mk_and a b = And [a;b]

let mk_ands = function
  | [] -> Bool true
  | [x] -> x
  | xs -> And xs

let mk_or a b = Or [a;b]

let mk_ors = function
  | [] -> Bool false
  | [x] -> x
  | x::xs -> List.fold_left xs ~init:x ~f:mk_or

let mk_pred pred as' = Pred (pred, as')

let rec mk_not' (negate_var : 'bvar -> 'bvar) = function
  | Var x  -> Var (negate_var x)
  | Bool b -> Bool (not b)
  | Or  fs -> And (List.map fs ~f:(mk_not' negate_var))
  | And fs -> Or  (List.map fs ~f:(mk_not' negate_var))
  | Pred(pred, as') -> Pred(negate_pred pred, as')
let mk_not f = mk_not' Void.absurd f

let mk_implies a b = mk_or (mk_not a) b


(* TODO This should be hidden *)
let cartesian_products : 'a list list -> 'a list list =
  fun xss ->
  List.fold_right xss ~init:[[]] ~f:begin fun xs acc ->
    List.map (List.cartesian_product xs acc) ~f:begin fun (y,ys) -> y::ys end
  end

let rec to_DNF : ('var, 'arith) gen_t -> ('var, 'arith) gen_t list list =
  fun f -> match f with
  | Var _ ->  [[f]]
  | Pred _ ->  [[f]]
  | Bool true -> [[]]
  | Bool false -> []
  | Or fs -> List.concat_map fs ~f:to_DNF
  | And fs ->
      let open List in
      map ~f:concat (cartesian_products (map fs ~f:to_DNF))

let rec fvs : ('bvar, 'avar) gen_t -> 'bvar list * 'avar list =
  function
    | Bool _ -> [], []
    | Var x  -> [x], []
    | Pred (_, as') -> [], List.concat_map as' ~f:Arith.fvs
    | Or fs' | And fs' ->
        let vss, avss = List.unzip @@ List.map fs' ~f:fvs in
        List.concat vss, List.concat avss

let lift f x y = match (x, y) with
  | Some(x), Some(y) -> Some(f x y)
  | _ -> None

let simplify_pred p args = 
  let int_or_none = List.map ~f:Arith.evaluate_opt args in
  match p, int_or_none with
  | Eq, [x; y] -> lift (=) x y
  | Neq, [x; y] -> lift (<>) x y
  | Le, [x; y] -> lift (<=) x y
  | Ge, [x; y] -> lift (>=) x y
  | Lt, [x; y] -> lift (<) x y
  | Gt, [x; y] -> lift (>) x y
  | _ -> failwith "simplify pred"
