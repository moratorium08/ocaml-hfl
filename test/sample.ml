open Hfl

let input =
"%HES
Sentry =v ∀x_369. FIB x_369 (\\x_358. true).
FIB n k_fib_22 =u
  (n >= 2 \\/ k_fib_22 1)
  /\\ (n < 2
  \\/ FIB (n - 1) (\\x_364. FIB (n - 2) (\\x_355. k_fib_22 (x_364 + x_355))))."

let raw_hes = Parse.from_string input

let hes =
  let (hes, _) = Raw_hflz.to_typed (raw_hes, []) in
  Hflz.desugar hes


let%expect_test "fibonacci" =
 Format.printf "%a" (Print.hflz_hes Print.simple_ty_) hes;
  [%expect{|
    ∀x_36910:int.FIB x_36910 (λx_35811:int.true)
    s.t.
    FIB : int -> (int -> bool) -> bool =μ
      λn12:int.
       λk_fib_2213:(int -> bool).
        (n12 >= 2 || k_fib_2213 1)
        && (n12 < 2
            || FIB (n12 - 1)
                (λx_36414:int.
                  FIB (n12 - 2) (λx_35515:int.k_fib_2213 (x_36414 + x_35515))))
    |}]

let%test "is not nuonly" =
  not @@ Hflz.is_nuonly hes

let%test "is not muonly" =
  not @@ Hflz.is_muonly hes
