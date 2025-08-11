open Hfl


let%expect_test "capture avoiding" =
  let input =
"%HES
S   =ν F 0.
F y =ν (\\x . ∀y. x = 0 || x >= 1 && F (x-1)) y."
  in
  let raw_hes = Parse.from_string input in
  let hes =
    let (hes, _) = Raw_hflz.to_typed (raw_hes, []) in
    Hflz.desugar hes
  in

  let top = Hflz.top_formula_of hes in
  let rule = List.hd @@ Hflz.equations_of hes in
  let rule = {rule with Hflz.body = Trans.Reduce.Hflz.beta rule.Hflz.body} in
  let hes = Hflz.mk_hes top [rule] in

 Format.printf "%a" (Print.hflz_hes Print.simple_ty_) hes;
  [%expect{|
    F 0
    s.t.
    F : int -> bool =ν λy39:int.∀y47:bool.y39 = 0 || y39 >= 1 && F (y39 - 1)
    |}]

let%expect_test "capture avoiding without parse" =
  let open Type in
  let open Hflz in
  let id_n n t = { Id.name = "x_" ^ string_of_int n; id = n; ty = t } in
  let tysbool = TySigma (TyBool ()) in
    (*  (\x. (\z. x /\ z)) (\z. z)  *)
    (* This is ill-typed, but we do not care  *)
  let phi =
      App (
        Abs (
          id_n 1 tysbool,
          Abs (
            id_n 2 tysbool,
            And (
              Var (id_n 1 (TyBool ())),
              Var (id_n 2 (TyBool ()))
            )
          )
        ),
        Abs (
          id_n 2 tysbool,
          Var (id_n 2 (TyBool ()))
        )
      )
  in
  Format.printf "Original: %a" (Print.hflz Print.simple_ty_)  phi;
  [%expect {| Original: (λx_11:bool.λx_22:bool.x_11 && x_22) (λx_22:bool.x_22) |}];
  let phi = Trans.Reduce.Hflz.beta phi in
  Format.printf "After beta: %a" (Print.hflz Print.simple_ty_)  phi;
    (*  \y. (\z. z) /\ y  *)
  [%expect {| After beta: λx_255:bool.(λx_258:bool.x_258) && x_255 |}]

let%expect_test "firstorder/simple-exists.in" =
  let input =
"%HES
Sentry =v ∀x2. x2 = 0 \\/ F x2.
F x3 =u ∃r4. r4 <> 0 /\\ x3 + r4 = 0."
  in
  let raw_hes = Parse.from_string input in
  let hes =
    let (hes, _) = Raw_hflz.to_typed (raw_hes, []) in
    Hflz.desugar hes
  in

  let hes = Trans.Reduce.Hflz.inline hes in

 Format.printf "%a" (Print.hflz_hes Print.simple_ty_) hes;
  [%expect{|
    ∀x266:int.
     x266 = 0 || (λx367:int.∃r468:int.r468 <> 0 && x367 + r468 = 0) x266
    s.t.
    |}]
