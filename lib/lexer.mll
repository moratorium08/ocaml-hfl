{
open Parser
}

let space = ['\t' '\r' ' ']
let newline = ['\n']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let alphanum = ['0'-'9' 'a'-'z' 'A'-'Z' '_']
let ope_symbols = [ '=' '<' '>' '+' '-' '*' '&' '|' '\\' '/' '!' '%']

rule token = parse
| newline                  { Lexing.new_line lexbuf; token lexbuf }
| space+                   { token lexbuf }
| "/*"                     { comment lexbuf; token lexbuf }
| "%LTS"                   { skip_all lexbuf; token lexbuf }
| eof                      { EOF       }
| "%HES"                   { START_HES }
| "("                      { LPAREN    }
| ")"                      { RPAREN    }
| "true"                   { TRUE      }
| "false"                  { FALSE     }
| "!" | "not"              { NOT       }
| ("\\"|"λ")               { LAMBDA    }
| ("=v"|"=ν")              { DEF_G     }
| ("=m"|"=u"|"=μ")         { DEF_L     }
| "."                      { DOT       }
| ":"                      { COLON     }
| ";"                      { SEMICOLON }
| "int"                    { TINT      }
| "bool"                   { TBOOL     }
| "->"                     { TARROW    }
| "∀"                      { FORALL    }
| "∃"                      { EXISTS    }
| digit digit*             { INT (int_of_string (Lexing.lexeme lexbuf)) }
| upper alphanum*          { UIDENT (Lexing.lexeme lexbuf) }
| lower alphanum*          { LIDENT (Lexing.lexeme lexbuf) }
| ope_symbols ope_symbols* { match Lexing.lexeme lexbuf with
                           | "+"           -> PLUS
                           | "-"           -> MINUS
                           | "*"           -> STAR
                           | "/"           -> SLASH 
                           | "%"           -> PERCENT
                           | "="           -> EQ
                           | "/="          -> NEQ
                           | "!="          -> NEQ
                           | "<>"          -> NEQ
                           | "<="          -> LE
                           | ">="          -> GE
                           | "<"           -> LANGRE
                           | ">"           -> RANGRE
                           | ("&&"|"/\\")  -> AND
                           | ("||"|"\\/")  -> OR
                           | "=>"          -> IMPLY
                           | "<=>"         -> IFF
                           | s -> failwith ("unknown operater " ^ s)
                           }

and comment = parse
| "*/"
    { () }
| "/*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { failwith "unterminated comment" }
| _
    { comment lexbuf }
and skip_all = parse
| eof { () }
| _   { skip_all lexbuf }

{
  (* This part is inserted into the end of the generated file. *)
}
