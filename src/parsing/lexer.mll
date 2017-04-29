{
open Parser

exception SyntaxError of string
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

rule read = parse
(* keywords *)
| "case" { CASE }
| "enum" { ENUM }
| "fn" { FN }
| "interface" { INTERFACE }
| "implementation" { IMPLEMENTATION }
| "match" { MATCH }

(* punctuation *)
| "->" { ARROW }
| ":" { COLON }
| "," { COMMA }
| "." { DOT }
| "=" { EQ }
| "_" { UNDERSCORE }
| "<" { L_ANGLE }
| ">" { R_ANGLE }
| "{" { L_BRACE }
| "}" { R_BRACE }
| "(" {
    match lexbuf.lex_start_pos with
    | 0 -> NL_L_PAREN
    | _ -> L_PAREN
  }
| newline "(" { Lexing.new_line lexbuf; NL_L_PAREN }
| ")" { R_PAREN }
| eof { EOF }

(* tokens with semantic values *)
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { UCID(Lexing.lexeme lexbuf) }
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { LCID(Lexing.lexeme lexbuf) }

(* numbers *)
| ['0'-'9']['0'-'9' '_']* { INT(int_of_string @@ Lexing.lexeme lexbuf) }

(* whitespace *)
| blank { read lexbuf }
| newline { Lexing.new_line lexbuf; read lexbuf }
| "/*" { comment 1 lexbuf }
| "//" { single_line_comment lexbuf }

and comment depth = parse
| "/*" { comment (depth + 1) lexbuf }
| "*/" { if depth = 1 then read lexbuf else comment (depth - 1) lexbuf }
| eof { raise (SyntaxError "Unterminated comment") }
| newline { Lexing.new_line lexbuf; comment depth lexbuf }
| _ { comment depth lexbuf }

and single_line_comment = parse
| newline { Lexing.new_line lexbuf; read lexbuf }
| eof { EOF }
| _ { single_line_comment lexbuf }
