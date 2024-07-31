(** Public exceptions used in the hfl library *)

exception Fatal of string
exception Lexing_error of string
exception Parse_error of string
exception Type_error of string

let fatal s = raise (Fatal s)
