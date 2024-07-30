(** Parsing module with custom error messages *)

exception LexingError of string
exception ParseError of string

val from_string : string -> Raw_hflz.hes

val from_channel : in_channel -> file_name:string -> Raw_hflz.hes
