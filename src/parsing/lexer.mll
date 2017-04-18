{
open Parser

exception SyntaxError of string
}

rule read = parse
(* keywords *)
| "fn" { FN }
| "enum" { ENUM }

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

(* numbers *)
| ['0'-'9']['0'-'9' '_']* { INT(int_of_string @@ Lexing.lexeme lexbuf) }

(* whitespace *)
| (" " | "\t") { read lexbuf }
| ("\n" | "\r" | "\r\n") { Lexing.new_line lexbuf; read lexbuf }
| "/*" { comment 1 lexbuf }
| "//" _* "\n" { Lexing.new_line lexbuf; read lexbuf }

and comment depth = parse
| "/*" { comment (depth + 1) lexbuf }
| "*/" { if depth = 1 then read lexbuf else comment (depth - 1) lexbuf }
| eof { raise (SyntaxError "Unterminated comment") }
| _ { comment depth lexbuf }
