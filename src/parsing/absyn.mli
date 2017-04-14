type name = string

type import
type export

type type_ =
  | Con of name
  | Arrow of type_ list * type_

type literal =
  | Int of int

type generic = {
  name : name;
  constraints : name list option;
}

type parameter = {
  name : name;
  type_ : type_;
}

type body = expr list

and function_ = {
  name : name option;
  generics : generic list option;
  parameters : parameter list;
  return_type : type_;
  body: body;
}

and application = {
  callee: expr;
  generic_arguments: type_ list option;
  arguments: expr list;
}

and expr =
  | Function of function_
  | Application of application
  | Var of name
  | Literal of literal
  | Unit

type program = {
  imports : import list;
  exports : export list;
  body : body;
}
