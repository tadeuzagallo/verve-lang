open React
open Lwt
open LTerm_text

let make_prompt state =
  let prompt = Printf.sprintf "> " in
  eval [ S prompt ]

(* Format the interpreter output for REPL display *)
let make_output value ty =
  Printer.print_raw Format.str_formatter value ty;
  eval [ S (Format.flush_str_formatter ()) ]

let eval (tenv, s1, venv) str =
  let lexbuf = Lexing.from_string (str ^ "\n") in
  let decl = Parser.decl_start Lexer.read lexbuf in
  let ty, tenv', s2 = Typing.check_decl tenv decl in
  let value, venv' = Interpreter.eval_decl venv decl in
  let subst = Typing.(s2 >> s1) in
  let ty' = Typing.(apply subst ty) in
  (tenv', subst, venv'), value, ty'

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.const (make_prompt state))
end

let rec loop term history state =
  Lwt.catch (fun () ->
    let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
    rl#run >|= fun command -> Some command
  ) (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    Lwt.catch
      (fun () ->
         let state, value, ty = eval state command in
         LTerm.fprintls term (make_output value ty)
         >>= fun () -> return state)
      (function
        | Type_error.Error e ->
          Type_error.report_error Format.str_formatter e;
          let err = LTerm_text.eval [S (Format.flush_str_formatter ())] in
          LTerm.fprintls term err >>= fun () -> return state
        | exn -> Lwt.fail exn)
    >>= fun state ->
      LTerm_history.add history command;
      loop term history state
  | None ->
    loop term history state

let main () =
  LTerm_inputrc.load () >>= fun () ->
    Lwt.catch (fun () ->
      let state = (Typing.default_env, [], []) in
      Lazy.force LTerm.stdout >>= fun term ->
        loop term (LTerm_history.create []) state
    ) (function
          | LTerm_read_line.Interrupt -> Lwt.return ()
          | exn -> Lwt.fail exn)