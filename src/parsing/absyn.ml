type export

type location = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
}

type name = {
  str : string;
  loc : location;
}

type qualified_name = name list

type type_ = {
  type_desc : type_desc;
  type_loc : location;
}

and type_desc =
  | Arrow of type_ list * type_
  | Inst of qualified_name * type_ list
  | RecordType of (name * type_) list

type literal =
  | Int of int
  | String of string

type generic = {
  name : name;
  constraints : qualified_name list;
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
  intf_name : name;
  intf_param : generic;
  intf_items : interface_item list;
}

and interface_item =
  | Prototype of prototype
  | OperatorPrototype of operator_prototype

and prototype = {
  proto_name : name;
  proto_generics : generic list;
  proto_params : type_ list;
  proto_ret_type : type_ option;
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
  oproto_name : name;
  oproto_rhs : type_;
  oproto_ret_type : type_ option;
}

and body = stmt list

and function_ = {
  fn_name : name option;
  fn_generics : generic list;
  fn_parameters : parameter list;
  fn_return_type : type_ option;
  fn_body: body;
}

and implementation = {
  impl_name : qualified_name;
  impl_arg : type_;
  impl_items : implementation_item list;
  mutable impl_arg_type : Types.texpr option;
}

and implementation_item =
  | ImplFunction of function_
  | ImplOperator of operator

and application = {
  callee: expr;
  generic_arguments: type_ list;
  arguments: expr list option;
  mutable generic_arguments_ty: Types.texpr list;
}

and 'a ctor = {
  ctor_name : qualified_name;
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
  case_value : stmt list;
}

and binop = {
  bin_lhs: expr;
  bin_op : name;
  bin_rhs : expr;
  mutable bin_generic_arguments_ty: Types.texpr list;
}

and operator = {
  op_attributes : attribute list;
  op_generics : generic list;
  op_lhs : parameter;
  op_name : name;
  op_rhs : parameter;
  op_ret_type : type_ option;
  op_body : body;
}

and expr = {
  expr_desc : expr_desc;
  expr_loc : location;
}

and expr_desc =
  | Function of function_
  | Application of application
  | Var of var
  | Ctor of expr ctor
  | Literal of literal
  | Record of (name * expr) list
  | Field_access of field_access
  | Match of match_
  | Binop of binop
  | If of if_
  | Unit
  | Wrapped of expr
  | ClassCtor of class_ctor
  | MethodCall of method_call

and method_call = {
  mc_object : expr;
  mc_method : name;
  mc_args : expr list;
}

and class_ctor = {
  cc_name : qualified_name;
  cc_generics : type_ list;
  cc_record : (name * expr) list;
}

and var = {
  var_name : qualified_name;
  mutable var_type : Types.texpr list;
}

and if_ = {
  if_cond : expr;
  if_conseq : body;
  if_alt : else_ option;
}

and else_ =
  | ElseIf of if_
  | ElseBlock of body

and let_ =  {
  let_var : name;
  let_value : expr;
}

and pattern = {
  pat_desc : pattern_desc;
  pat_loc : location;
}
and pattern_desc =
  | Pany
  | Pvar of name
  | Pctor of qualified_name * pattern list option

and stmt = {
  stmt_desc : stmt_desc;
  stmt_loc : location;
}
and stmt_desc =
  | Let of let_
  | FunctionStmt of function_
  | Expr of expr

and decl = {
  decl_desc : decl_desc;
  decl_loc : location;
}
and decl_desc =
  | Enum of enum
  | Stmt of stmt
  | Interface of interface
  | TypeAlias of type_alias
  | Implementation of implementation
  | Class of class_
  | Operator of operator

and class_ = {
  class_name : name;
  class_generics : generic list;
  class_props : class_prop list;
  class_fns : function_ list;
}

and class_prop = {
  cp_name : name;
  cp_type : type_;
}

and import = {
  i_loc : location;
  i_global : bool;
  i_module : name list;
  i_alias : name option;
  i_items : import_item list option
}

and import_item =
  | ImportValue of name
  | ImportType of name * name list option

and type_alias = {
  ta_name : name;
  ta_generics : generic list;
  ta_type : type_;
}

type program = {
  imports : import list;
  exports : export list;
  body : decl list;
}

let dummy_loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos }

let mk_name str = { loc = dummy_loc; str }
let mk_qualified_name str = List.map mk_name str

let fn_of_operator op = {
  fn_name = Some (op.op_name);
  fn_generics = op.op_generics;
  fn_parameters = [ op.op_lhs; op.op_rhs ];
  fn_return_type = op.op_ret_type;
  fn_body = op.op_body;
}

let app_of_binop binop = {
  callee = { expr_desc = Var { var_name = [binop.bin_op]; var_type = [] }; expr_loc = binop.bin_op.loc };
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
