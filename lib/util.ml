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
