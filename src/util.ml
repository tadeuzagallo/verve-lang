open Lexing

module A = Absyn

let default_env = (Env.default_env, Rt_env.default_env, [])

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
  with
  | Type_error.Error e ->
    Type_error.report_error Format.err_formatter e;
    env, true
  | Runtime_error.Error e ->
    Runtime_error.report_error Format.err_formatter e;
    env, true

let rec eval_file ?(print=true) ?(state=(default_env, false)) = with_file @@ fun (file, input) ->
  let ast = parse input in
  let state = List.fold_left (import file) state ast.Absyn.imports in
  List.fold_left (eval_decl ~print) state ast.Absyn.body

and import file ((tenv,_,_), _ as state) import =
  try
    let filename = resolve_import file import.Absyn.i_module in
    let ((tenv', venv, nenv), has_error) = eval_file ~print:false ~state filename in
    let filtered = import_filter tenv' import.Absyn.i_items in
    let tenv' = alias tenv filtered import in
    ((tenv', venv, nenv), has_error)
  with Type_error.Error e ->
    Type_error.report_error Format.err_formatter e;
    let (st, _) = state in (st, true)

and alias tenv tenv' import =
  if import.A.i_global then Env.merge tenv' tenv else
  match import.A.i_alias with
    | None -> Env.add_module tenv (List.hd @@ List.rev import.A.i_module) tenv'
    | Some name -> Env.add_module tenv name tenv'

and import_filter tenv = function
  | None -> tenv
  | Some items ->
    let add_ctors env = function
      | None -> env
      | Some names ->
        let aux env name =
          let ctor = Env.find_ctor tenv [name] in
          Env.add_ctor env name ctor
        in List.fold_left aux env names
    in
    let aux env = function
      | Absyn.ImportValue v ->
        let value = Env.find_value tenv [v] in
        Env.add_value env v value
      | Absyn.ImportType (t, ctors) ->
        let ty = Env.find_type tenv [t] in
        let env' = Env.add_type env t ty in
        add_ctors env' ctors
    in
    List.fold_left aux Env.empty items

and resolve_import file mod_ =
  let parts = List.map (fun n -> n.Absyn.str) mod_ in
  let modname = String.concat Filename.dir_sep parts ^ ".vrv" in
  Filename.concat (Filename.dirname file) modname
