open Base
open Type

let rec subst_names_arith : [`Int] Id.t IdMap.t -> Arith.t -> Arith.t =
  fun env e ->
  match e with
  | Int _ -> e
  | Var v ->
     begin match IdMap.find env v with
     | None -> e
     | Some v' -> Var v'
     end
  | Op(op, es) -> Op(op, List.map ~f:(subst_names_arith env) es)

let rec hflz' : 'ty arg Id.t IdMap.t -> 'ty Hflz.t -> 'ty Hflz.t =
    (* env : old_name |-> new_name *)
    fun env phi -> match phi with
    | Var x -> begin
        match IdMap.find env x with
        | Some v -> begin
            (* 'ty Type.arg -> 'ty *)
            let ty =
              (match v.Id.ty with
              | TySigma ty -> ty
              | TyInt -> assert false)
            in
            Var {v with ty}
          end
        | None -> Var x
      end
    | Or (phi1, phi2) ->
       Or (hflz' env phi1, hflz' env phi2)
    | And (phi1, phi2) ->
       And (hflz' env phi1, hflz' env phi2)
    | App (phi1, phi2) ->
       App (hflz' env phi1, hflz' env phi2)
    | Abs (x', t) ->
       (* Generates a new variable,
          and shadows the variable if it was already used.
          We use the same name, but new id *)
       let x = { x' with id = Id.gen_id () } in
       Abs (x, hflz' (IdMap.replace env x' x) t)
    | Forall (x', t) ->
       let x = { x' with id = Id.gen_id () } in
       Forall (x, hflz' (IdMap.replace env x' x) t)
    | Exists (x', t) ->
       let x = { x' with id = Id.gen_id () } in
       Exists (x, hflz' (IdMap.replace env x' x) t)
    | Bool b -> Bool b
    | Arith e  ->
       (* 'ty arg IdMap.t -> [`Int] IdMap.t *)
       let env' =
         Map.filter_map
           env
           ~f:(fun a ->
             match a.Id.ty with
                | TyInt -> Some {a with ty=`Int}
                | TySigma _ -> None
              )
          in
          Arith (subst_names_arith env' e)
        | Pred (p, es) ->
          let env' =
            Map.filter_map
              env
              ~f:(fun a ->
                match a.Id.ty with
                | TyInt -> Some {a with ty=`Int}
                | TySigma _ -> None
              )
          in
          Pred (p, List.map es ~f:(subst_names_arith env'))

let hflz phi = hflz' IdMap.empty phi

let hes_rule rule =
  {rule with Hflz.body = hflz rule.Hflz.body}

let hes phi =
  let top = Hflz.top_formula_of phi in
  let rules = Hflz.equations_of phi in
  let top = hflz top in
  let rules = List.map ~f:hes_rule rules in
  Hflz.mk_hes top rules
