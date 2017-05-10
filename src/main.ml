open Lexing
open Cmdliner
open Lwt
open Fmt

(*
 * verve fmt foo.vrv
 * verve compile foo.vrv
 * verve dump-ast foo.vrv
 * verve
 *)

(* Helper functions *)
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
  try Parser.program Lexer.read lexbuf with
  | _ ->
    Printf.fprintf Pervasives.stderr "%a: syntax error at '%s'\n" print_position lexbuf (Lexing.lexeme lexbuf);
    exit (-1)

(* Command functions *)
let run_fmt = with_file @@ fun (_, input) ->
  let ast = parse input in
  Printer.Absyn.pp_program Format.std_formatter ast

let run_compile = ()

let run_dump_ast = ()

let rec eval_file ~state ~print = with_file @@ fun (file, input) ->
  let eval (state, has_error) decl = try
      let state', value, ty = Repl.eval state decl in
      if print then Printer.print value ty;
      state', has_error
    with Type_error.Error e ->
      Type_error.report_error Format.err_formatter e;
      Format.pp_print_newline Format.err_formatter ();
      state, true
  in
  let ast = parse input in
  let state = List.fold_left (import file) state ast.Absyn.imports in
  List.fold_left eval state ast.Absyn.body

and import file state import =
  let filename = resolve_import file import.Absyn.i_module in
  eval_file ~state ~print:false filename

and resolve_import file mod_ =
  let parts = List.map (fun n -> n.Absyn.str) mod_ in
  let modname = String.concat Filename.dir_sep parts ^ ".vrv" in
  Filename.concat (Filename.dirname file) modname

let run_file file =
  let env = (Env.default_env, Interpreter.default_env, []) in
  let _, err = eval_file ~state:(env, false) ~print:true file in
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
    Printf.fprintf Pervasives.stderr "%s" (Format.flush_str_formatter ())
  | status -> Term.exit status
