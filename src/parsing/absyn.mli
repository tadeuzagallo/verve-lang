type generic_parameters = (string * string list option) list
type parameters = (string * string) list
type return_type = string
type body = expr list
and expr = E of string * generic_parameters * parameters * return_type * body
