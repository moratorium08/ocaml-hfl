(** Modules for arithemetic expressions. *)

(** The grammar for arithemetic operations ia [e ::= n | x | op(x_1, ..., x_n)]. *)

(** Artithmetic operations *)
type op = Add | Sub | Mult | Div | Mod

(** Derived functions: *)

val equal_op : op -> op -> bool
val compare_op : op -> op -> int
val pp_op : Format.formatter -> op -> unit
val show_op : op -> string
val iter_op : op -> unit
val map_op : op -> op
val fold_op : 'a -> op -> 'a
val op_of_sexp : Sexplib0.Sexp.t -> op
val sexp_of_op : op -> Sexplib0.Sexp.t


(** Arithmetic expresion parametrized by variable type *)
type 'var gen_t =
    Int of Base.int
  | Var of 'var
  | Op of op * 'var gen_t Base.list

(* Derived functins: *)

val equal_gen_t :
  ('var -> 'var -> bool) ->
  'var gen_t -> 'var gen_t -> bool
val compare_gen_t :
  ('var -> 'var -> int) ->
  'var gen_t -> 'var gen_t -> int
val pp_gen_t :
  (Format.formatter -> 'var -> unit) -> Format.formatter ->
  'var gen_t -> unit
val show_gen_t :
  (Format.formatter -> 'var -> unit) -> 'var gen_t -> string
val iter_gen_t : ('var -> unit) -> 'var gen_t -> unit
val map_gen_t : ('a -> 'b) -> 'a gen_t -> 'b gen_t
val fold_gen_t : ('a -> 'b -> 'a) -> 'a -> 'b gen_t -> 'a
val gen_t_of_sexp :
  (Sexplib0.Sexp.t -> 'var) -> Sexplib0.Sexp.t -> 'var gen_t
val sexp_of_gen_t :
  ('var -> Sexplib0.Sexp.t) -> 'var gen_t -> Sexplib0.Sexp.t

type t = ([ `Int ] Id.t) gen_t

(** Derived functions: *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string
val iter : 'a -> unit
val map : 'a -> 'a
val fold : 'a -> 'b -> 'a
val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t

(** Constructors: *)

val mk_int : int -> 'a gen_t
val mk_op : op -> 'a gen_t list -> 'a gen_t
val mk_var' : 'a -> 'a gen_t

(** Specific to [t] *)
val mk_var : 'a Id.t -> t
val fvs : 'var gen_t -> 'var list
val lift : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

(** Turns the binary operator into the corresponding OCaml function *)
val op_func : op -> int -> int -> int
val evaluate_opt : 'a gen_t -> int option
