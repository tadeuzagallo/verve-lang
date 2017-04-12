open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse file =
  let lexbuf = Lexing.from_channel file in
  try Parser.program Lexer.read lexbuf with
  | _ ->
    Printf.fprintf stderr "%a: syntax error at '%s'\n" print_position lexbuf (Lexing.lexeme lexbuf);
    exit (-1)

let () =
  let ast = parse stdin in
  List.iter (fun exp ->
    let v = Interpreter.eval exp in
    Printer.print_expr stderr v
  ) ast.Absyn.body
