open Absyn
open Printf

let print_list sep printer out =
  let rec aux = function
    | x :: y :: rest ->
        printer out x;
        fprintf out "%s" sep;
        aux (y :: rest)
    | x :: [] ->
        printer out x
    | [] -> ()
  in aux

let print_generic out { name = TVar name; constraints } =
  fprintf out "%s" name;
  match constraints with
  | None -> ()
  | Some constraints' ->
      fprintf out ": (%a)"
        (print_list ", " (fun out x -> fprintf out "%s" x)) constraints'

let print_type out = function
  | Con t -> fprintf out "%s" t
  | _ -> assert false (* TODO *)

let print_param out { name; type_ } =
  fprintf out "%s: %a" name print_type type_

let print_maybe out = function
  | Some str -> fprintf out "%s" str
  | None -> ()

let rec print_fn out { name; generics; parameters; return_type; body } = 
  fprintf out "fn %a <%a>(%a) -> %a { %a }\n"
    print_maybe name
    (print_list ", " print_generic) generics
    (print_list ", " print_param) parameters
    print_type return_type
    (print_list "\n" print_expr) body

and print_expr out = function
  | Function fn -> print_fn out fn

let print_program p =
  List.iter (print_expr stderr) p.body
