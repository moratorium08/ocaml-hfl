open Base
open Type
open TransUtil

type 'x env = 'x IdMap.t
module Id__ = struct
  let rec arith : [`Int ] Id.t env -> Arith.t -> Arith.t =
    fun env a ->
      match a with
      | Int _ -> a
      | Var v ->
        begin match IdMap.find env v with
          | None -> a
          | Some v' -> Var v'
        end
      | Op(op, as') -> Op(op, List.map ~f:(arith env) as')

  let rec formula : [`Int ] Id.t IdMap.t -> Formula.t -> Formula.t =
    fun env p ->
    match p with
        | Pred(prim, as') -> Pred(prim, List.map as' ~f:(arith env))
        | And ps -> And(List.map ~f:(formula env) ps)
        | Or  ps -> Or (List.map ~f:(formula env) ps)
        | _ -> p

end

  (* TODO IdMapを使う *)
module Arith__ = struct
  let rec arith_
    : ('var -> 'var -> bool)
      -> 'var
      -> 'var Arith.gen_t
      -> 'var Arith.gen_t
      -> 'var Arith.gen_t =
    fun equal x a a' ->
      match a' with
      | Int _ -> a'
      | Var x' -> if equal x x' then a else a'
      | Op(op, as') -> Op(op, List.map ~f:(arith_ equal x a) as')
  let arith : 'a. 'a Id.t -> Arith.t -> Arith.t -> Arith.t =
    fun x a a' -> arith_ Id.eq {x with ty=`Int} a a'

  let rec formula_
    : ('var -> 'var -> bool)
      -> 'var
      -> 'var Arith.gen_t
      -> ('bvar,'var) Formula.gen_t
      -> ('bvar,'var) Formula.gen_t =
    fun equal x a p ->
    match p with
    | Pred(prim, as') -> Pred(prim, List.map as' ~f:(arith_ equal x a))
    | And ps -> And(List.map ~f:(formula_ equal x a) ps)
    | Or  ps -> Or (List.map ~f:(formula_ equal x a) ps)
    | _ -> p
  let formula : 'a. 'a Id.t -> Arith.t -> Formula.t -> Formula.t =
    fun x a p -> formula_ Id.eq {x with ty = `Int} a p

end

module Hflz__ = struct

  let rec arith : 'ty Hflz.t env -> Arith.t -> Arith.t =
    fun env a -> match a with
      | Int _ -> a
      | Var x ->
        begin match IdMap.find env x with
          | None -> a
          | Some (Arith a') -> a'
          | _ -> assert false
        end
      | Op(op, as') -> Op(op, List.map ~f:(arith env) as')

  let rec hflz : 'ty Hflz.t env -> 'ty Hflz.t -> 'ty Hflz.t =
    fun env phi -> match phi with
      | Var x ->
        begin match IdMap.lookup env x with
          | t -> t
          | exception e -> Var x
        end
      | Or(phi1,phi2)  -> Or(hflz env phi1, hflz env phi2)
      | And(phi1,phi2) -> And(hflz env phi1, hflz env phi2)
      | App(phi1,phi2) -> App(hflz env phi1, hflz env phi2)
      | Abs(x, t)      -> Abs(x, hflz (IdMap.remove env x) t)
      | Forall(x, t)   -> Forall(x, hflz (IdMap.remove env x) t)
      | Exists(x, t)   -> Exists(x, hflz (IdMap.remove env x) t)
      | Arith a        -> Arith (arith env a)
      | Pred (p,as')   -> Pred(p, List.map ~f:(arith env) as')
      | Bool _         -> phi
end

module Id = Id__
module Arith = Arith__
module Hflz = Hflz__
