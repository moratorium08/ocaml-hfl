(* gets hes_rule list. returns hes_rule list and toplevel name*)
let translate_top top_rule =
  let rec inner = function
    | Hflz.Abs(x, y) -> Hflz.Forall(x, inner y)
    | x -> x
  in
  let open Hflz in
  let open Type in
  let body = inner top_rule.body in
  (* remove arguments of the template type *)
  let var = {top_rule.var with ty=TyBool(())} in
  {top_rule with body; var}

let main psi = match psi with
  | [] -> [], None
  | top::xs -> translate_top top::xs, Some(top.var)
