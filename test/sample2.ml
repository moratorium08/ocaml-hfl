open Hfl

(* From Rethfl's example 1.in *)

let input =
"%HES
S   =v X n || n < 0.
X y =v y = 0 || y >= 1 && X (y-1)."

let raw_hes = Parse.from_string input

let hes =
  let (hes, _) = Raw_hflz.to_typed (raw_hes, []) in
  Hflz.desugar hes


let%expect_test "1.in" =
 Format.printf "%a" (Print.hflz_hes Print.simple_ty_) hes;
  [%expect{|
    X n3 || n3 < 0
    s.t.
    X : int -> int -> bool =ν λn3:int.λy4:int.y4 = 0 || y4 >= 1 && X (y4 - 1)
    |}]

let%test "is nuonly" =
  Hflz.is_nuonly hes

let%test "is not muonly" =
  not @@ Hflz.is_muonly hes
