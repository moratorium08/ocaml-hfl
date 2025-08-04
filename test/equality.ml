open Hfl

let%test "equal_simpl_ty" =
  let open Type in
  let ty1 = TyArrow ({name = "x"; id = 1; ty = TyInt}, TyBool ()) in
  let ty2 = TyArrow ({name = "y"; id = 2; ty = TyInt}, TyBool ()) in
  equal_simple_ty ty1 ty2
