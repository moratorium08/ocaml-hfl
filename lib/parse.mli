(** Parsing module with custom error messages *)

(** @raise Exception.Parse_error
    @raise Exception.Lexing_error *)
val from_string : string -> Raw_hflz.hes

(** @raise Exception.Parse_error
    @raise Exception.Lexing_error *)
val from_channel : in_channel -> file_name:string -> Raw_hflz.hes
