{
open Parser

exception SyntaxError of string
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

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
| blank { read lexbuf }
| newline { Lexing.new_line lexbuf; EOL }
| "/*" { comment 1 lexbuf }
| "//" { single_line_comment lexbuf }

and comment depth = parse
| "/*" { comment (depth + 1) lexbuf }
| "*/" { if depth = 1 then read lexbuf else comment (depth - 1) lexbuf }
| eof { raise (SyntaxError "Unterminated comment") }
| _ { comment depth lexbuf }

and single_line_comment = parse
| newline { read lexbuf }
| eof { EOF }
| _ { single_line_comment lexbuf }
