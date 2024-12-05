open Hfl

let%expect_test "fibonacci" =
  let input =
"%HES
Sentry =v ∀x_369. FIB x_369 (\\x_358. true).
FIB n k_fib_22 =u
  (n >= 2 \\/ k_fib_22 1)
  /\\ (n < 2
  \\/ FIB (n - 1) (\\x_364. FIB (n - 2) (\\x_355. k_fib_22 (x_364 + x_355))))."
  in
  let raw_hes = Parse.from_string input in
  let (hes, _) = Raw_hflz.to_typed (raw_hes, []) in
  let hes = Hflz.desugar hes in
  Format.printf "%a" (Print.hflz_hes Print.simple_ty_) hes;
  [%expect{|
    ∀x_3693:int.FIB x_3693 (λx_3584:int.true)
    s.t.
    FIB : int -> (int -> bool) -> bool =μ
      λn5:int.
       λk_fib_226:(int -> bool).
        (n5 >= 2 || k_fib_226 1)
        && (n5 < 2
            || FIB (n5 - 1)
                (λx_3647:int.
                  FIB (n5 - 2) (λx_3558:int.k_fib_226 (x_3647 + x_3558))))
    |}]
