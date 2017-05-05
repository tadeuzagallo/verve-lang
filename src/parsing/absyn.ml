type name = string

type import
type export

type type_ =
  | Arrow of type_ list * type_
  | Inst of name * type_ list
  | RecordType of (name * type_) list

type literal =
  | Int of int
  | String of string

type generic = {
  name : name;
  constraints : name list;
}

type parameter = {
  param_name : name;
  param_type : type_;
}

type enum_item =
  | EnumCtor of enum_ctor
  | EnumOp of enum_op

and enum_ctor = {
  enum_ctor_name : name;
  enum_ctor_generics : generic list;
  enum_ctor_parameters : type_ list option;
}

and enum_op = {
  enum_op_lhs : type_;
  enum_op_op : name;
  enum_op_rhs : type_;
}

and enum = {
  enum_name : name;
  enum_generics : generic list; 
  enum_items : enum_item list;
}

and interface = {
  intf_name : string;
  intf_param : generic;
  intf_items : interface_item list;
}

and interface_item =
  | Prototype of prototype
  | OperatorPrototype of operator_prototype

and prototype = {
  proto_name : string;
  proto_generics : generic list;
  proto_params : type_ list;
  proto_ret_type : type_;
}

and attribute = {
  attr_name : name;
  attr_value : attribute_value option;
}

and attribute_value =
  | AttrOp of name
  | AttrInt of int
  | Attribute of attribute

and operator_prototype = {
  oproto_attributes : attribute list;
  oproto_generics : generic list;
  oproto_lhs : type_;
  oproto_name : string;
  oproto_rhs : type_;
  oproto_ret_type : type_;
}

and body = expr list

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
  impl_items : implementation_item list;
  mutable impl_arg_type : Types.ty option;
}

and implementation_item =
  | ImplFunction of function_
  | ImplOperator of operator

and application = {
  callee: expr;
  generic_arguments: type_ list;
  arguments: expr list option;
  mutable generic_arguments_ty: Types.ty list;
}

and 'a ctor = {
  ctor_name : name;
  ctor_generic_arguments : type_ list;
  ctor_arguments : 'a list option;
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

and binop = {
  bin_lhs: expr;
  bin_op : string;
  bin_rhs : expr;
  mutable bin_generic_arguments_ty: Types.ty list;
}

and operator = {
  op_attributes : attribute list;
  op_generics : generic list;
  op_lhs : parameter;
  op_name : string;
  op_rhs : parameter;
  op_ret_type : type_;
  op_body : body;
}

and expr =
  | Function of function_
  | Operator of operator
  | Application of application
  | Var of name
  | Ctor of expr ctor
  | Literal of literal
  | Record of (name * expr) list
  | Field_access of field_access
  | Match of match_
  | Binop of binop
  | Unit
  | Wrapped of expr

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

let fn_of_operator op = {
  fn_name = Some (op.op_name);
  fn_generics = op.op_generics;
  fn_parameters = [ op.op_lhs; op.op_rhs ];
  fn_return_type = op.op_ret_type;
  fn_body = op.op_body;
}

let app_of_binop binop = {
  callee = Var binop.bin_op;
  generic_arguments = [];
  arguments = Some [ binop.bin_lhs; binop.bin_rhs ];
  generic_arguments_ty = binop.bin_generic_arguments_ty;
}

let prototype_of_op_proto op = {
  proto_name = op.oproto_name;
  proto_generics = op.oproto_generics;
  proto_params = [ op.oproto_lhs; op.oproto_rhs ];
  proto_ret_type = op.oproto_ret_type;
}
