open Base

type 'ty t =
  { name : string
  ; id   : int
  ; ty   : 'ty
  }
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let eq x y = String.equal x.name y.name && x.id = y.id

let id_counter = new Util.counter
let gen_id () = id_counter#tick

let to_string id =
  let c = String.get id.name 0 in
  if Char.equal c (Char.uppercase c)
  then id.name
  else id.name ^ Int.to_string id.id

let gen : ?name:string -> 'annot -> 'anno t =
  fun ?(name="x") ann ->
     { name = name
     ; id = gen_id()
     ; ty = ann
     }

let remove_ty : 'ty t -> unit t = fun x -> { x with ty = () }

module Key = struct
  type nonrec t = unit t
  let sexp_of_t = sexp_of_t sexp_of_unit
  let t_of_sexp = t_of_sexp unit_of_sexp
  let compare : t -> t -> int = compare Base.Unit.compare
  let hash  (x : t) : int = String.hash  (to_string x)
  include (val Comparator.make ~compare ~sexp_of_t)
end
