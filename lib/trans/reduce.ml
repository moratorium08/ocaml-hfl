open Base
open Type
open TransUtil

module S = TransUtil.ModuleWrapper

module Hflz = struct
  let rec beta : 'a S.Hflz.t -> 'a S.Hflz.t = function
    | Or (phi1, phi2) -> Or (beta phi1, beta phi2)
    | And(phi1, phi2) -> And(beta phi1, beta phi2)
    | App(phi1, phi2) ->
      begin match beta phi1, beta phi2 with
        | Abs(x, phi1), phi2 -> beta @@ Subst.Hflz.hflz (IdMap.of_list [x,phi2]) phi1
        | phi1, phi2 -> App(phi1, phi2)
      end
    | Abs(x, phi) -> Abs(x, beta phi)
    | phi -> phi
  let rec ones = 1 :: ones
  module Scc(Key: Comparator.S) = struct
    module NodeSet = Set.M(Key)
    module NodeMap = Map.M(Key)
    type graph = NodeSet.t NodeMap.t
    let rg : graph -> graph = fun g ->
      Map.fold g ~init:(Map.empty (module Key)) ~f:begin fun ~key ~data:set map ->
        let map' =
          if Map.mem map key
          then map
          else Map.add_exn map ~key ~data:(Set.empty (module Key))
        in
        Set.fold set ~init:map' ~f:begin fun map v ->
          let data =
            match Map.find map v with
            | Some s -> Set.add s key
            | None   -> Set.singleton (module Key) key
          in
          Map.replace map ~key:v ~data
        end
      end
    let rec dfs : graph -> NodeSet.t -> Key.t list -> graph * Key.t list =
      fun g ls r ->
      Set.fold ls ~init:(g,r) ~f:begin fun (g,r) x ->
        match Map.find g x with
        | None -> g, r
        | Some s ->
          let g3, r3 = dfs (Map.remove g x) (Set.remove s x) r in
          g3, x::r3
      end
    let rec rdfs : graph -> Key.t -> Key.t list -> graph * Key.t list =
      fun g v ls ->
      match Map.find g v with
      | None   -> g, ls
      | Some s ->
        Set.fold s ~init:(Map.remove g v, v :: ls) ~f:begin fun (rg,ls) v ->
          rdfs rg v ls
        end
    let scc g =
      let rG = rg g in
      let map, vs = dfs g (Set.of_list (module Key) @@ Map.keys g) [] in
      let _, ls =
        List.fold vs ~init:(rG, []) ~f:begin fun (rg,ls) v ->
          let rg2, l = rdfs rg v [] in
          if List.is_empty l
          then rg2, ls
          else rg2, l::ls
        end
      in
      ls
  end
  let inline : simple_ty S.Hflz.hes -> simple_ty S.Hflz.hes =
    fun (main, rules) ->
    let module Scc = Scc(Id.Key) in
    let fpreds_of_main = S.Hflz.fpreds main in
    let dep_graph : Scc.graph =
      Map.of_alist_exn (module Id.Key)  @@ List.map rules ~f:begin fun rule ->
        let id = rule.var in
        let dep = S.Hflz.fpreds rule.body
        in Id.remove_ty id ,dep
      end
    in
    let mutual_recursives =
      Scc.scc dep_graph
      |> List.filter ~f:(fun x -> List.length x > 1)
      |> List.concat
      |> IdSet.of_list
    in
    let rules, inlinables =
      List.partition_tf rules ~f:begin fun rule ->
        IdSet.mem mutual_recursives rule.var ||
        IdSet.mem (Hflz.fvs rule.body) rule.var
      end
    in
    let inlinables = (* topologically sort *)
      let topological_ord =
        Set.fold fpreds_of_main ~init:(dep_graph, []) ~f:begin fun (g, vs) v ->
          if not (List.mem vs v ~equal:(fun x y -> Id.Key.compare x y = 0)) then
            Scc.rdfs g v vs
          else
            g, vs
        end
        |> snd
        |> List.rev
        |> List.enumerate
        |> Map.of_alist_exn (module Id.Key)
      in
      List.sort inlinables ~compare:begin fun x y ->
        let value (z : 'a Hflz.hes_rule) =
          (* Not found when [z] is unused non-terminal *)
          Option.value ~default:0 @@ IdMap.find topological_ord z.var
        in
        Int.compare (value x) (value y)
      end
    in
    Log.info begin fun m -> m ~header:"Inline" "%a"
        Print.(list_comma id) (List.map inlinables ~f:(fun x -> x.var))
    end;
    let inline_map =
      let rules_in_map =
        Map.of_alist_exn (module Id.Key) @@ List.map inlinables ~f:begin fun rule ->
          Id.remove_ty rule.var, rule.body
        end
      in
      List.fold_left inlinables ~init:rules_in_map ~f:begin fun map rule ->
        let var  = rule.var in
        let body = IdMap.lookup map var in
        let map = IdMap.map map ~f:(Subst.Hflz.hflz (IdMap.singleton var body)) in
        Log.debug begin fun m ->
          let pp ppf (x,psi) = Print.(pf ppf "    %a = %a" id x (hflz simple_ty_) psi) in
          m ~header:"Inline" "%a inlined:@.@[<v>%a@]"
            Print.id rule.var
            Print.(list pp) (IdMap.to_alist map)
        end;
        map
      end
    in
    let simplified_rules = List.map rules ~f:begin fun rule ->
        { rule with body = Subst.Hflz.hflz inline_map rule.body }
      end
    in
    let simplified_main = Subst.Hflz.hflz inline_map main in
    (simplified_main, simplified_rules)
end
