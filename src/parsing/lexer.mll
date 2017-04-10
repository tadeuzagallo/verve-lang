{
open Parser
}

rule read = parse
(* keywords *)
| "fn" { FN }

(* punctuation *)
| "->" { ARROW }
| ":" { COLON }
| "," { COMMA }
| "<" { L_ANGLE }
| ">" { R_ANGLE }
| "{" { L_BRACE }
| "}" { R_BRACE }
| "(" { L_PAREN }
| ")" { R_PAREN }
| eof { EOF }

(* tokens with semantic values *)
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { UCID(Lexing.lexeme lexbuf) }
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { LCID(Lexing.lexeme lexbuf) }

(* whitespace *)
| (" " | "\t") { read lexbuf }
| ("\n" | "\r" | "\r\n") { Lexing.new_line lexbuf; read lexbuf }
