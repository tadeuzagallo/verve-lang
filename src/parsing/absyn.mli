type name = string

type import
type export

type tvar = TVar of name

type type_ =
  | Var of tvar
  | Con of name
  | Arrow of type_ * type_

type generic = {
  name : tvar;
  constraints : name list option;
}

type parameter = {
  name : name;
  type_ : type_;
}

type body = expr list

and function_ = {
  name : name option;
  generics : generic list;
  parameters : parameter list;
  return_type : type_;
  body: body;
}

and expr =
  | Function of function_

type program = {
  imports : import list;
  exports : export list;
  body : body;
}
