open Lexing

let print_list sep printer out =
  let rec aux = function
    | x :: y :: rest ->
        printer out x;
        Printf.fprintf out "%s" sep;
        aux (y :: rest)
    | x :: [] ->
        printer out x
    | [] -> ()
  in aux

let print_generic out (t, ts) =
  Printf.fprintf out "%s" t;
  match ts with
  | None -> ()
  | Some ts' ->
      Printf.fprintf out ": (%a)"
        (print_list ", " (fun out x -> Printf.fprintf out "%s" x)) ts'

let print_param out (p, t) =
  Printf.fprintf out "%s: %s" p t

let rec print_fn out (Absyn.E (name, generic, params, ret, body)) = 
  Printf.fprintf out "fn %s <%a>(%a) -> %s { %a }\n"
    name
    (print_list ", " print_generic) generic
    (print_list ", " print_param) params
    ret
    (print_list "\n" print_fn) body

let print_exps = List.iter (print_fn stderr)

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
  print_exps ast
