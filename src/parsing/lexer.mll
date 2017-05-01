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
    if lexbuf.lex_last_pos = lexbuf.lex_start_p.pos_bol
    then NL_L_PAREN
    else L_PAREN
  }
| newline blank* "(" { Lexing.new_line lexbuf; NL_L_PAREN }
| ")" { R_PAREN }
| eof { EOF }

(* tokens with semantic values *)
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { UCID(Lexing.lexeme lexbuf) }
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { LCID(Lexing.lexeme lexbuf) }

(* literals *)
| ['0'-'9']['0'-'9' '_']* { INT(int_of_string @@ Lexing.lexeme lexbuf) }
| '"' { string (Buffer.create 32) lexbuf }

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
| newline blank* { Lexing.new_line lexbuf; read lexbuf }
| eof { EOF }
| _ { single_line_comment lexbuf }

and string buf = parse
| "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
| '"' { STRING(Buffer.contents buf) }
| (newline | eof) { raise (SyntaxError "String is not terminated") }
| _ { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0); string buf lexbuf }
