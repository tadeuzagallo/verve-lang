type name = string

type import
type export

type tvar = TVar of name

type type_ =
  | Var of tvar
  | Con of name
  | Arrow of type_ list * type_

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

and application = {
  callee: expr;
  generic_arguments: type_ list;
  arguments: expr list;
}

and expr =
  | Function of function_
  | Application of application
  | Var of name

type program = {
  imports : import list;
  exports : export list;
  body : body;
}
