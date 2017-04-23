open Absyn
open Printf

module V = Value

let print_literal out = function
  | Int i -> fprintf out "%d" i

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

let print_generic out { name; constraints } =
  fprintf out "%s" name;
  match constraints with
  | [] -> ()
  | [ x ] -> fprintf out ": %s" x
  | constraints' ->
      fprintf out ": (%a)"
        (print_list ", " (fun out x -> fprintf out "%s" x)) constraints'

let rec print_type out = function
  | Con t -> fprintf out "%s" t
  | Arrow (ps, r) ->
      fprintf out "(%a) -> %a"
        (print_list "," print_type) ps
        print_type r
  | Inst (n, ts) ->
      fprintf out "%s<%a>" n (print_list ", " print_type) ts

let print_param out { param_name; param_type } =
  fprintf out "%s: %a" param_name print_type param_type

let print_maybe out = function
  | Some str -> fprintf out "%s" str
  | None -> ()

let print_generics out = function
  | [] -> ()
  | generics ->
      fprintf out "<%a>" (print_list ", " print_generic) generics

let rec print_fn out { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  fprintf out "fn %a %a(%a) -> %a { %a }\n"
    print_maybe fn_name
    print_generics fn_generics
    (print_list ", " print_param) fn_parameters
    print_type fn_return_type
    (print_list "\n" print_expr) fn_body

and print_generic_arguments out = function
  | [] -> ()
  | args ->
      fprintf out "<%a>" (print_list ", " print_type) args

and print_app out { callee; generic_arguments; arguments } =
  fprintf out "%a(%a)\n"
    print_expr callee
    (print_list ", " print_expr) arguments

and print_ctor out { ctor_name;  ctor_generic_arguments; ctor_arguments } =
  let print_opt_args out = function
    | None -> ()
    | Some args ->
        fprintf out "(%a)" (print_list ", " print_expr) args
  in
  fprintf out "%s%a%a"
    ctor_name
    print_generic_arguments ctor_generic_arguments
    print_opt_args ctor_arguments

and print_expr out = function
  | Function fn -> print_fn out fn
  | Ctor ctor -> print_ctor out ctor
  | Application app -> print_app out app
  | Var str -> fprintf out "%s" str
  | Literal l -> fprintf out "%a" print_literal l
  | Unit -> fprintf out "()"

and print_value out = function
  | V.Function fn -> print_fn out fn
  | V.Ctor ctor -> print_ctor out ctor
  | V.Literal l -> fprintf out "%a" print_literal l
  | V.Unit -> fprintf out "()"
  | V.Type t -> fprintf out "%s" t

let print expr ty =
  fprintf stderr "%a : %s\n"
    print_value expr
    (Types.to_string ty)
