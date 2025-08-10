open Base

type 'ty arg
  = TyInt
  | TySigma of 'ty
  [@@deriving eq,ord,show,iter,map,fold,sexp]

type 'annot ty
  = TyBool of 'annot
  | TyArrow of 'annot ty arg Id.t * 'annot ty
  [@@deriving eq,ord,show,iter,map,fold,sexp]

type 'annot arg_ty = 'annot ty arg
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let unsafe_unlift = function
  | TyInt -> invalid_arg "unsafe_unlift"
  | TySigma ty -> ty

let lift_arg x = Id.{ x with ty = TySigma x.ty }

let rec order = function
  | TyBool _ -> 0
  | TyArrow({ty=arg_ty; _}, return_ty) ->
      let order_arg =
        match arg_ty with
        | TyInt -> 0
        | TySigma arg_ty -> order arg_ty
      in
      max (order_arg + 1) (order return_ty)

(* Simple Type *)

type simple_ty = unit ty
  [@@deriving ord,show,sexp]

(* We do not derive eq as the derived eq uses eq for Id.t *)
let rec equal_simple_ty ty1 ty2 =
  match ty1, ty2 with
  | TyBool _, TyBool _ -> true
  | TyArrow ({ty=ty1;_}, cod1), TyArrow({ty=ty2;_}, cod2) -> begin
    let is_dom_eq =
      match ty1, ty2 with
      | TySigma ty1', TySigma ty2' -> equal_simple_ty ty1' ty2'
      | TyInt, TyInt -> true
      | _ -> false in
    is_dom_eq && equal_simple_ty cod1 cod2
  end
  | _ -> false

type simple_argty = simple_ty arg
  [@@deriving eq,ord,show,sexp]

let to_simple x = map_ty (fun _ -> ()) x

let mk_arrows args ret_ty =
  List.fold_right args ~init:ret_ty ~f:begin fun arg ret_ty ->
    TyArrow(arg, ret_ty)
  end

let decompose_arrow : 'annot ty -> 'annot ty arg Id.t list * 'annot =
  let rec go acc = function
    | TyBool ann -> List.rev acc, ann
    | TyArrow (x, ty) -> go (x::acc) ty
  in fun x -> go [] x

let rec merge append ty1 ty2 =
  match ty1, ty2 with
    | TyBool a1, TyBool a2 -> TyBool (append a1 a2)
    | TyArrow ({ty=TyInt;_} as x1, rty1)
    , TyArrow ({ty=TyInt;_} as x2, rty2) when Id.eq x1 x2 ->
        TyArrow ( x1, merge append rty1 rty2 )
    | TyArrow ({ty=TySigma aty1;_} as x1, rty1)
    , TyArrow ({ty=TySigma aty2;_} as x2, rty2) when Id.eq x1 x2 ->
        TyArrow
          ( {x1 with ty = TySigma (merge append aty1 aty2)}
          , merge append rty1 rty2 )
    | _ -> invalid_arg "Type.merge"

let merges append = function
    | [] -> invalid_arg "Type.merges"
    | ty::tys -> List.fold_right ~init:ty tys ~f:(merge append)
