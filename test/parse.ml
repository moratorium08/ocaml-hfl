open Hfl

let%expect_test "no %HES" =
  let input =
"Sentry =v ∀x_369. FIB x_369 (\\x_358. true).
FIB n k_fib_22 =u
  (n >= 2 \\/ k_fib_22 1)
  /\\ (n < 2
  \\/ FIB (n - 1) (\\x_364. FIB (n - 2) (\\x_355. k_fib_22 (x_364 + x_355))))."
  in
  begin
    try
      let _ = Parse.from_string input in
      print_string "Parse OK"
    with
      Exception.Parse_error emsg -> print_string emsg
    | _ -> print_string "unexpected error"
  end;
  [%expect{|
    Parse Error at 1:6:
    Cousumed input:

    Current LR(1) state: <some initial state>
    |}]


let%expect_test "uppercase variable" =
  let input =
"%HES
Sentry =v ∀ x . -1 * x >= 0 \\/ P x 0.
P x y =v
  ( x >= y+1 \\/ x <= 1 \\/ ( P ( -1 + x ) 0 /\\ WF RF1 x y (x - 1) 0 )) /\\ (
  y >= x \\/ ( P x ( 1 + y ) /\\ WF RF2 x y x (y + 1))
  ) .
WF RF x y x2 y2 =v ∀ r. ∀ r2. RF x y r \\/ RF x2 y2 r2 \\/ r >= 0 /\\ r > r2.
RF1 x y r =v r <> x.
RF2 x y r =v r <> -y."
  in
  begin
    try
      let _ = Parse.from_string input in
      print_string "Parse OK"
    with
      Exception.Parse_error emsg -> print_string emsg
    | _ -> print_string "unexpected error"
  end;
  [%expect{|
    Parse Error at 7:5:
    Cousumed input:
     START_HES hflz_rule hflz_rule uvar
    Current LR(1) state: 3
    hflz_rule -> uvar . list_lvar_ def_fixpoint hflz DOT
    |}]
