open Lexing

let default_env = (Env.default_env, Interpreter.default_env, [])

let with_file fn file =
  let in_ch = open_in file in
  let ret = fn (file, in_ch) in
  close_in in_ch;
  ret

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse file =
  let lexbuf = Lexing.from_channel file in
  try Parser.program Lexer.read lexbuf
  with _ ->
    Printf.fprintf Pervasives.stderr "%a: syntax error at '%s'\n" print_position lexbuf (Lexing.lexeme lexbuf);
    exit (-1)

let eval_decl ~print (env, has_error) decl =
  try
    let env', value, ty = Repl.eval env decl in
    if print then Printer.print value ty;
    env', has_error
  with Type_error.Error e ->
    Type_error.report_error Format.err_formatter e;
    Format.pp_print_newline Format.err_formatter ();
    env, true

let rec eval_file ?(print=true) ?(state=(default_env, false)) = with_file @@ fun (file, input) ->
  let ast = parse input in
  let state = List.fold_left (import file) state ast.Absyn.imports in
  List.fold_left (eval_decl ~print) state ast.Absyn.body

and import file state import =
  let filename = resolve_import file import.Absyn.i_module in
  eval_file ~print:false ~state filename

and resolve_import file mod_ =
  let parts = List.map (fun n -> n.Absyn.str) mod_ in
  let modname = String.concat Filename.dir_sep parts ^ ".vrv" in
  Filename.concat (Filename.dirname file) modname
