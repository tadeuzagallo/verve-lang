{
open Parser

exception SyntaxError of string
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let operator = ['/' '=' '-' '+' '!' '*' '%' '<' '>' '&' '|' '^' '~' '?']

rule read = parse
(* keywords *)
| "case" { CASE }
| "enum" { ENUM }
| "fn" { FN }
| "interface" { INTERFACE }
| "implementation" { IMPLEMENTATION }
| "let" { LET }
| "match" { MATCH }
| "operator" { OPERATOR }
| "type" { TYPE }
| "global" { GLOBAL }
| "import" { IMPORT }
| "as" { AS }
| "if" { IF }
| "else" { ELSE }
| "class" { CLASS }

(* punctuation *)
| "->" { ARROW }
| ":" { COLON }
| "," { COMMA }
| "." { DOT }
| "=" { EQ }
| "#" { HASH }
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

(* tokens with semantic values *)
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { UCID(Lexing.lexeme lexbuf) }
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* "'"* { LCID(Lexing.lexeme lexbuf) }

(* literals *)
| ['0'-'9']['0'-'9' '_']* { INT(int_of_string @@ Lexing.lexeme lexbuf) }
| '"' { read_string (Buffer.create 32) lexbuf }

(* whitespace *)
| blank { read lexbuf }
| newline { Lexing.new_line lexbuf; read lexbuf }

(* comments *)
| "/*" { comment 1 lexbuf }
| "//" { single_line_comment lexbuf }
| '/' (operator # ['/' '*'])* | (operator # ['/']) operator* { OP(Lexing.lexeme lexbuf) }

| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }

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

and read_string buf =
parse
| '"'       { STRING (Buffer.contents buf) }
| '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }
