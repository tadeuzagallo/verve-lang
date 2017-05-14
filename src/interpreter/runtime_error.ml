open Fmt

type error =
   | Unbound_variable of Absyn.qualified_name
   | Unknown of string

exception Error of error

let error err = raise (Error err)

let report_error ppf = function
  | Unbound_variable name ->
    pf ppf "RuntimeError: Unknown variable: %a\n" Printer.Absyn.pp_qualified_name name
  | Unknown msg ->
    pf ppf "RuntimeError: %s\n" msg
