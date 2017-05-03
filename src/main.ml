open Lexing
open Cmdliner
open Lwt

(*
 * verve fmt foo.vrv
 * verve compile foo.vrv
 * verve dump-ast foo.vrv
 * verve
 *)

(* Helper functions *)
let with_file fn file =
  let in_ch = open_in file in
  let ret = fn in_ch in
  close_in in_ch;
  ret

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

(* Command functions *)
let run_fmt = with_file @@ fun file ->
  let ast = parse file in
  Printer.Absyn.pp_program Format.std_formatter ast

let run_compile = ()

let run_dump_ast = ()

let run_file = with_file @@ fun file ->
  let eval (state, has_error) decl = try
      let state', value, ty = Repl.eval state decl in
      Printer.print value ty;
      state', has_error
    with Type_error.Error e ->
      Type_error.report_error Format.err_formatter e;
      Format.pp_print_newline Format.err_formatter ();
      state, true
  in
  let ast = parse file in
  let state = (Env.default_env, [], Interpreter.default_env, []) in
  let _, err = List.fold_left eval (state, false) ast.Absyn.body in
  if err then exit 1

let run_repl () =
  Lwt_main.run (Repl.main ())

let run_run = function
  | [] -> run_repl()
  | [f] -> run_file f
  | _ -> failwith "More than one argument"

(* Common options *)
let path =
  let doc = "The input file" in
  Arg.(last & pos_all file [] & info [] ~doc ~docv:"path")

let file =
  let doc = "The input file" in
  Arg.(last & pos_all non_dir_file [] & info [] ~doc ~docv:"file")

let opt_file =
  let doc = "The input file" in
  Arg.(value & pos_all non_dir_file [] & info [] ~doc ~docv:"file")

(* Commands *)
let fmt =
  let doc = "Pretty print a file" in
  let info = Term.info "fmt" ~doc in
  let fmt_t = Term.(const run_fmt $ path) in
  (fmt_t, info)

let compile =
  let doc = "Compile a verve file to a native executable" in
  let info = Term.info "compile" ~doc in
  let compile_t = Term.(const run_compile) in
  (compile_t, info)

let dump_ast =
  let doc = "Dump the Abstract Syntax Tree (AST) of a verve file" in
  let info = Term.info "dump-ast" ~doc in
  let dump_ast_t = Term.(const run_dump_ast) in
  (dump_ast_t, info)

let run =
  let doc = "Evaluate a Verve source file" in
  let info = Term.info "verve" ~doc in
  let run_t = Term.(const run_run $ opt_file) in
  (run_t, info)

(* Entry point *)
let () =
  let cmds = [ fmt; compile; dump_ast ] in
  match Term.eval_choice ~err:Format.str_formatter run cmds with
  | `Error `Parse -> Term.(exit @@ eval run)
  | `Error _ ->
    Printf.fprintf stderr "%s" (Format.flush_str_formatter ())
  | status -> Term.exit status
