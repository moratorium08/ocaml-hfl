let check_hflz phi =
  let open Hflz in
  let open Type in
  let error str = raise (Exception.Type_error str) in
  let pp = Print.hflz Print.simple_ty_ in
  let rec go phi =
    match phi with
    | Bool   _ -> TyBool ()
    | Var    v -> v.ty
    | Or (f1, f2)  -> begin
        let t1 = go f1 in
        if not @@ equal_simple_ty t1 (TyBool ())
        then
          error (Fmt.str "%a should have type bool" pp f1)
        else
          let t2 = go f2 in
          if not @@ equal_simple_ty t2  (TyBool ())
          then
            error (Fmt.str "%a should have type bool" pp f2)
          else
            TyBool ()
      end
    | And (f1, f2) -> begin
        let t1 = go f1 in
        if not @@ equal_simple_ty t1 (TyBool ())
        then
          error (Fmt.str "Expression %a should have type bool" pp f1)
        else
          let t2 = go f2 in
          if not @@ equal_simple_ty t2 (TyBool ())
          then
            error (Fmt.str "Expression %a should have type bool" pp f2)
          else
            TyBool ()
      end
    | Abs (x, f1)  -> TyArrow (x, go f1)
    | Forall (_, f1) -> begin
      let ty = go f1 in
      if not @@ equal_simple_ty ty (TyBool ())
      then
        error (Fmt.str "Expression %a should have type bool" pp f1)
      else
        TyBool ()
      end
    | Exists (_, f1) -> begin
      let ty = go f1 in
      if not @@ equal_simple_ty ty (TyBool ())
      then
        error (Fmt.str "Expression %a should have type bool" pp f1)
      else
        TyBool ()
      end
    | App (f1, f2)   -> begin
      let ty1 = go f1 in
      match ty1 with
      | TyArrow (x, ty1') -> begin
        (match x.ty with
         | TyInt ->
            (match f2 with
               Arith _ -> ()
             | _ -> error (Fmt.str "Illegal type (App, Arrow) (ty1=TyInt, ty2=(not integet expression)) (expression: %a)" pp phi))
         | TySigma t -> (
           let sty2 = go f2 in
           if not @@ equal_simple_ty t sty2
           then
             error (Fmt.str "Type assertion failed for expression %a" pp phi)
         )
        );
        ty1'
      end
      | _ -> error (Fmt.str "Error in expression %a" pp phi)

    end
    | Pred _ -> TyBool ()
    | Arith _ -> error (Fmt.str "An arithmetic expression %a cannot appear here" pp  phi)
  in go phi
