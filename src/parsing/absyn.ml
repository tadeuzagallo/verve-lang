type name = string

type import
type export

type type_ =
  | Con of name
  | Arrow of type_ list * type_
  | Inst of name * type_ list

type literal =
  | Int of int

type generic = {
  name : name;
  constraints : name list option;
}

type parameter = {
  param_name : name;
  param_type : type_;
}

type enum_item = {
  enum_item_name : name;
  enum_item_generics : generic list option;
  enum_item_parameters : type_ list option;
}

type enum = {
  enum_name : name;
  enum_generics : generic list option;
  enum_items : enum_item list;
}

type body = expr list

and function_ = {
  fn_name : name option;
  fn_generics : generic list option;
  fn_parameters : parameter list;
  fn_return_type : type_;
  fn_body: body;
}

and application = {
  callee: expr;
  generic_arguments: type_ list option;
  arguments: expr list;
}

and ctor = {
  ctor_name : name;
  ctor_generic_arguments : type_ list option;
  ctor_arguments : expr list option;
}

and expr =
  | Function of function_
  | Application of application
  | Var of name
  | Ctor of ctor
  | Literal of literal
  | Unit

and decl =
  | Enum of enum
  | Expr of expr

type program = {
  imports : import list;
  exports : export list;
  body : decl list;
}
