type error =
   | Unbound_variable of Absyn.qualified_name
   | Unknown of string

exception Runtime_error of error

let error err = raise (Runtime_error err)
