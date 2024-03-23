(* This is a private module *)

(* Extension of some Base modules *)
module List = struct
  include Base.List
  let enumerate xs =
    zip_exn xs (init (length xs) ~f:(fun x -> x))
end

module Map = struct
  include Base.Map
  let replace map ~key ~data =
        let map = remove map key in
        add_exn map ~key ~data
end

class counter = object
  val mutable cnt = 0
  method tick =
    let x = cnt in
    cnt <- x + 1;
    x
end

module Void = struct
  type t = Void of { absurd : 'a. 'a }
  let absurd (Void void) = void.absurd
  let equal v _     = absurd v
  let compare v _   = absurd v
  let pp _ v        = absurd v
  let t_of_sexp _   = failwith "void_of_sexp"
  let sexp_of_t v   = absurd v
end
