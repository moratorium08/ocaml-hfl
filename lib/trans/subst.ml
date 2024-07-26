open Base
open Type
open TransUtil

module S = TransUtil.ModuleWrapper

type 'x env = 'x IdMap.t
module Id = struct
  let rec arith : [`Int ] S.Id.t env -> S.Arith.t -> S.Arith.t =
    fun env a ->
      match a with
      | Int _ -> a
      | Var v ->
        begin match IdMap.find env v with
          | None -> a
          | Some v' -> Var v'
        end
      | Op(op, as') -> Op(op, List.map ~f:(arith env) as')

  let rec formula : [`Int ] S.Id.t IdMap.t -> S.Formula.t -> S.Formula.t =
    fun env p ->
    match p with
        | Pred(prim, as') -> Pred(prim, List.map as' ~f:(arith env))
        | And ps -> And(List.map ~f:(formula env) ps)
        | Or  ps -> Or (List.map ~f:(formula env) ps)
        | _ -> p

end

  (* TODO IdMapを使う *)
module Arith = struct
  let rec arith_
    : ('var -> 'var -> bool)
      -> 'var
      -> 'var S.Arith.gen_t
      -> 'var S.Arith.gen_t
      -> 'var S.Arith.gen_t =
    fun equal x a a' ->
      match a' with
      | Int _ -> a'
      | Var x' -> if equal x x' then a else a'
      | Op(op, as') -> Op(op, List.map ~f:(arith_ equal x a) as')
  let arith : 'a. 'a S.Id.t -> S.Arith.t -> S.Arith.t -> S.Arith.t =
    fun x a a' -> arith_ S.Id.eq {x with ty=`Int} a a'

  let rec formula_
    : ('var -> 'var -> bool)
      -> 'var
      -> 'var S.Arith.gen_t
      -> ('bvar,'var) S.Formula.gen_t
      -> ('bvar,'var) S.Formula.gen_t =
    fun equal x a p ->
    match p with
    | Pred(prim, as') -> Pred(prim, List.map as' ~f:(arith_ equal x a))
    | And ps -> And(List.map ~f:(formula_ equal x a) ps)
    | Or  ps -> Or (List.map ~f:(formula_ equal x a) ps)
    | _ -> p
  let formula : 'a. 'a S.Id.t -> S.Arith.t -> S.Formula.t -> S.Formula.t =
    fun x a p -> formula_ S.Id.eq {x with ty = `Int} a p

end

module Hflz = struct

  let rec arith : 'ty S.Hflz.t env -> S.Arith.t -> S.Arith.t =
    fun env a -> match a with
      | Int _ -> a
      | Var x ->
        begin match IdMap.find env x with
          | None -> a
          | Some (Arith a') -> a'
          | _ -> assert false
        end
      | Op(op, as') -> Op(op, List.map ~f:(arith env) as')

  let rec hflz : 'ty S.Hflz.t env -> 'ty S.Hflz.t -> 'ty S.Hflz.t =
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

  (** Invariant: phi must have type TyBool *)
  let reduce_head : 'ty S.Hflz.hes_rule list -> 'ty S.Hflz.t -> 'ty S.Hflz.t =
    fun hes phi -> match phi with
      | Var x ->
        begin match x.ty, List.find hes ~f:(fun rule -> S.Id.eq x rule.var) with
          | TyBool _, Some phi -> phi.body
          | _ -> invalid_arg "reduce_head"
          end
      | App(_, _) ->
          let head, args = S.Hflz.decompose_app phi in
          let vars, body =
            match S.Hflz.decompose_abs head with
            | vars0, Var x ->
              let x_rule =
                List.find_exn hes ~f:(fun rule -> S.Id.eq x rule.var)
              in
              let vars1, body = S.Hflz.decompose_abs x_rule.body in
              vars0@vars1, body
            | vars, body -> vars, body
          in
          let env = IdMap.of_list @@ List.zip_exn vars args in
          hflz env body
      | _ -> invalid_arg "reduce_head"
end
