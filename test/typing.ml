open Hfl

let%test "order bool" =
   Type.order (Type.TyBool()) = 0

let%test "order int -> bool" =
   let ty = Type.TyArrow ({name = "x"; id = 1; ty=TyInt}, TyBool()) in
   Type.order ty = 1

let%test "order int -> int -> bool" =
   let ty' = Type.TyArrow ({name = "y"; id = 2; ty=TyInt}, TyBool()) in
   let ty = Type.TyArrow ({name = "x"; id = 1; ty=TyInt}, ty') in
   Type.order ty = 1

let%test "order (int -> bool) -> bool" =
   let ty' = Type.TyArrow ({name = "x"; id = 1; ty=TyInt}, TyBool()) in
   let ty = Type.TyArrow ({name = "f"; id = 2; ty=TySigma ty'}, TyBool()) in
   Type.order ty = 2

(* We only have test cases for type erros since the other tests should cover
   the cases for well-typed formulas *)

let%expect_test "type_error01" =
  let input =
    "%HES
    Sentry =v RF 1 (-1) 0 (-1) 0.
    RF x r =v r <> -x + 2."
  in
  let raw_hes = Parse.from_string input in
  begin
    try
      let _ = Raw_hflz.to_typed (raw_hes, []) in
      print_string "Type Check OK"
    with
      Exception.Type_error emsg -> print_string emsg
    | _ -> print_string "unexpected error"
  end;
  [%expect{|
    FAIL (tv3@int -> (tv2@int -> (tv1@int -> o))) := o
    ill-typed
    |}]


let%expect_test "type_error02" =
  let input =
 "%HES
Sentry =v ∀x_369. FIB x_369 (\\x_358. true).
FIB n k_fib_22 =u
  (n >= 2 \\/ k_fib_22 1)
  /\\ (n < 2
  \\/ FIB (n - false) (\\x_364. FIB (n - 2) (\\x_355. k_fib_22 (x_364 + x_355))))."
  in
  let raw_hes = Parse.from_string input in
  begin
    try
      let _ = Raw_hflz.to_typed (raw_hes, []) in
      print_string "Type Check OK"
    with
      Exception.Type_error emsg -> print_string emsg
    | _ -> print_string "unexpected error"
  end;
  [%expect{| term (Raw_hflz.Bool false) is expected to be an arithmetic expression |}]
