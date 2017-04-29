type name = string

type import
type export

type type_ =
  | Arrow of type_ list * type_
  | Inst of name * type_ list
  | RecordType of (name * type_) list

type literal =
  | Int of int

type generic = {
  name : name;
  constraints : name list;
}

type parameter = {
  param_name : name;
  param_type : type_;
}

type enum_item = {
  enum_item_name : name;
  enum_item_generics : generic list;
  enum_item_parameters : type_ list option;
}

type enum = {
  enum_name : name;
  enum_generics : generic list; 
  enum_items : enum_item list;
}

type interface = {
  intf_name : string;
  intf_param : generic;
  intf_functions : prototype list;
}

and prototype = {
  proto_name : string;
  proto_generics : generic list;
  proto_params : type_ list;
  proto_ret_type : type_;
}

type body = expr list

and function_ = {
  fn_name : name option;
  fn_generics : generic list;
  fn_parameters : parameter list;
  fn_return_type : type_;
  fn_body: body;
}

and implementation = {
  impl_name : string;
  impl_arg : type_;
  impl_functions : function_ list;
  mutable impl_arg_type : Types.ty option;
}

and application = {
  callee: expr;
  generic_arguments: type_ list;
  arguments: expr list option;
  mutable generic_arguments_ty: Types.ty list;
}

and ctor = {
  ctor_name : name;
  ctor_generic_arguments : type_ list;
  ctor_arguments : expr list option;
}

and field_access = {
  record : expr;
  field : name;
}

and match_ = {
  match_value : expr;
  cases : match_case list;
}

and match_case = {
  pattern : pattern;
  case_value : expr list;
}

and expr =
  | Function of function_
  | Application of application
  | Var of name
  | Ctor of ctor
  | Literal of literal
  | Record of (name * expr) list
  | Field_access of field_access
  | Match of match_
  | Unit

and pattern =
  | Pany
  | Pvar of name
  | Pctor of name * pattern list option

and decl =
  | Enum of enum
  | Expr of expr
  | Interface of interface
  | Implementation of implementation

type program = {
  imports : import list;
  exports : export list;
  body : decl list;
}
