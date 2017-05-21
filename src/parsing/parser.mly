%{
open Absyn
open Parsing

let mk_loc () = { loc_start = symbol_start_pos(); loc_end = symbol_end_pos() }
let mk_name str = { str; loc = mk_loc() }
let mk_expr d = { expr_desc = d; expr_loc = mk_loc() }
let mk_type d = { type_desc = d; type_loc = mk_loc() }
let mk_pat d = { pat_desc = d; pat_loc = mk_loc() }
let mk_stmt d = { stmt_desc = d; stmt_loc = mk_loc() }
let mk_decl d = { decl_desc = d; decl_loc = mk_loc() }
let mk_var v = Var { var_name = v; var_type = [] }

%}

/* keywords */
%token CASE
%token ENUM
%token FN
%token INTERFACE
%token IMPLEMENTATION
%token LET
%token MATCH
%token OPERATOR
%token TYPE
%token GLOBAL
%token IMPORT
%token AS
%token IF
%token ELSE
%token CLASS

/* punctuation */
%token ARROW
%token COLON
%token COMMA
%token DOT
%token EQ
%token HASH
%token UNDERSCORE
%token L_ANGLE
%token R_ANGLE
%token L_BRACE
%token R_BRACE
%token L_PAREN
%token R_PAREN
%token NL_L_PAREN
%token EOF

/* tokens with semantic values */
%token <string> LCID
%token <string> UCID
%token <string> OP
%token <int> INT
%token <string> STRING

%start <Absyn.program> program
%start <Absyn.decl> decl_start

%nonassoc EMPTY_CTOR
%nonassoc CTOR_NO_ARGS

%nonassoc BELOW_PAREN
%left L_PAREN L_ANGLE L_BRACE R_ANGLE OP

%%

/* Helpers */

%inline angles(x): delimited(L_ANGLE, x, R_ANGLE) { $1 }
%inline braces(x): delimited(L_BRACE, x, R_BRACE) { $1 }
%inline parens(x): delimited(L_PAREN, x, R_PAREN) { $1 }

%inline plist(x): parens(separated_list(COMMA, x)) { $1 }
%inline blist(x): braces(separated_list(COMMA, x)) { $1 }

%inline nonempty_plist(x): parens(separated_nonempty_list(COMMA, x)) { $1 }
%inline nonempty_alist(x): angles(separated_nonempty_list(COMMA, x)) { $1 }

/* Entry points */
program: import* decl* EOF {
  { imports = $1; exports = []; body = $2 }
}

decl_start: decl EOF { $1 }

/* high-level structures */
decl: decl_desc { mk_decl $1 }
decl_desc:
  | enum { $1 }
  | interface { $1 }
  | implementation { $1 }
  | type_alias { $1 }
  | class_ { $1 }
  | operator { Operator $1 }
  | stmt { Stmt $1 }

stmt: stmt_desc { mk_stmt $1 }
stmt_desc:
  | let_ { $1 }
  | function_ { FunctionStmt $1 }
  | expr_desc_ { Expr (mk_expr $1) }

expr: expr_desc { mk_expr $1 }
expr_desc:
  | function_ { Function $1 }
  | expr_desc_ { $1 }

expr_desc_:
  | class_constructor { $1 }
  | constructor { $1 }
  | record { Record $1 }
  | match_expr { $1 }
  | if_ { If $1 }
  | binop { $1 }
  | atom_desc %prec BELOW_PAREN { $1 }

%inline atom: atom_desc { mk_expr $1 }
atom_desc:
  | lcid_name { mk_var $1 }
  | literal { Literal $1 }
  | method_call { $1 }
  | field_access %prec BELOW_PAREN { $1 }
  | application { $1 }
  (* Matched when the first expression in a sequence is wrapped in parens *)
  | parens(expr) { Wrapped $1 }
  | parens(op) { mk_var [$1] }
  (* Matched to break applications when there's a line break *)
  | NL_L_PAREN expr R_PAREN { Wrapped $2 }
  | NL_L_PAREN op R_PAREN { mk_var [$2] }

/* function expressions */
function_: FN lcid generic_parameters plist(parameter) return_type braces(list(stmt)) {
  { fn_name = Some ($2); fn_generics = $3; fn_parameters = $4; fn_return_type = $5; fn_body = $6 }
}

return_type: option(ARROW type_ { $2 }) { $1 }

generic_parameters: loption(nonempty_alist(generic_parameter)) { $1 }

generic_parameter: ucid loption(COLON quantifiers { $2 }) {
  { name = $1; constraints = $2 }
}

quantifiers:
  | ucid_name { [$1] }
  | plist(ucid_name) { $1 }

parameter: lcid COLON type_ {
  { param_name = $1; param_type = $3 }
}

/* types */
type_: type_desc { mk_type $1 }

type_desc:
  | ucid_name generic_arguments { Inst ($1, $2) }
  | arrow_type { $1 }
  | record_type { $1 }

arrow_type: plist(type_) ARROW type_ { Arrow($1, $3) }

record_type: blist(record_type_field) { RecordType $1 }

record_type_field: lcid COLON type_ { ($1, $3) }

/* application */

%inline application:
  | second_application { $1 }
  | first_application_desc %prec BELOW_PAREN { $1 }

%inline application_(x):
  | x plist(expr) {
    Application { callee = $1; generic_arguments = []; arguments = Some $2; generic_arguments_ty = [] }
  }
  | x generic_arguments_strict {
    Application { callee = $1; generic_arguments = $2; arguments = None; generic_arguments_ty = [] }
  }

second_application: application_(first_application) { $1 }

%inline first_application: first_application_desc { mk_expr $1 }
first_application_desc: application_(atom) { $1 }

generic_arguments: loption(generic_arguments_strict) { $1 }

generic_arguments_strict: nonempty_alist(type_) { $1 }

/* literals */
literal:
  | INT { Int $1 }
  | STRING { String $1 }

/* enums */
enum: ENUM ucid generic_parameters braces(nonempty_list(enum_item)) {
  Enum { enum_name = $2; enum_generics = $3; enum_items = $4 }
}

enum_item: ucid generic_parameters nonempty_plist(type_)? {
  { enum_item_name = $1; enum_item_generics = $2; enum_item_parameters = $3; }
}

%inline empty_constructor: ucid_name {
  Ctor { ctor_name = $1; ctor_generic_arguments = []; ctor_arguments = None }
}

%inline constructor_no_args: ucid_name generic_arguments_strict {
  Ctor { ctor_name = $1; ctor_generic_arguments = $2; ctor_arguments = None }
}

%inline constructor_args: ucid_name nonempty_plist(expr) {
    Ctor { ctor_name = $1; ctor_generic_arguments = []; ctor_arguments = Some $2 }
}

%inline constructor_full: ucid_name generic_arguments_strict nonempty_plist(expr) {
  Ctor { ctor_name = $1; ctor_generic_arguments = $2; ctor_arguments = Some $3 }
}

%inline constructor:
  | empty_constructor %prec EMPTY_CTOR { $1 }
  | constructor_no_args %prec CTOR_NO_ARGS { $1 }
  | constructor_args { $1 }
  | constructor_full { $1 }

/* interfaces */
interface: INTERFACE ucid angles(generic_parameter) braces(list(interface_item)) {
  Interface { intf_name = $2; intf_param = $3; intf_items = $4; }
}

interface_item:
  | prototype {$1}
  | operator_prototype {$1}

prototype: FN lcid generic_parameters plist(type_) return_type {
  Prototype {
    proto_name = $2;
    proto_generics = $3;
    proto_params = $4;
    proto_ret_type = $5;
  }
}

operator_prototype: attributes OPERATOR generic_parameters parens(type_) op parens(type_) return_type {
    OperatorPrototype {
      oproto_attributes = $1;
      oproto_generics = $3;
      oproto_lhs = $4;
      oproto_name = $5;
      oproto_rhs = $6;
      oproto_ret_type = $7;
    }
}

/* implementations */
implementation: IMPLEMENTATION ucid_name angles(type_) braces(list(impl_item)) {
  Implementation { impl_name = $2; impl_arg = $3; impl_items = $4; impl_arg_type = None }
}

impl_item:
  | function_ { ImplFunction $1 }
  | operator { ImplOperator $1 }

/* Records */

%inline record: blist(record_field) { $1 }
record_field: lcid COLON expr { ($1, $3) }

%inline field_access: atom DOT lcid {
  Field_access { record = $1; field = $3; }
}

/* pattern matching */
match_expr: MATCH expr braces(nonempty_list(match_case)) {
  Match { match_value = $2; cases = $3 }
}

match_case: CASE pattern COLON nonempty_list(stmt) {
  { pattern = $2; case_value = $4 }
}

pattern: pattern_desc { mk_pat $1 }
pattern_desc:
  | UNDERSCORE { Pany }
  | lcid { Pvar ($1) }
  | ucid_name option(plist(pattern)) { Pctor ($1, $2) }

/* binary operations */
binop: expr op expr %prec R_ANGLE {
  Binop { bin_lhs = $1; bin_op = $2; bin_rhs = $3; bin_generic_arguments_ty = [] }
}

operator: attributes OPERATOR generic_parameters parens(parameter) op parens(parameter) return_type braces(list(stmt)) {
    {
      op_attributes = $1;
      op_generics = $3;
      op_lhs = $4;
      op_name = $5;
      op_rhs = $6;
      op_ret_type = $7;
      op_body = $8;
    }
}

/* attributes */
attributes: list(HASH attribute { $2 }) { $1 }

attribute: lcid parens(attribute_value)? { { attr_name = $1; attr_value = $2 } }

attribute_value:
  | op { AttrOp $1 }
  | attribute { Attribute $1 }
  | INT { AttrInt $1 }

/* let binding */
%inline let_: LET lcid EQ expr {
  Let {
    let_var = $2;
    let_value = $4;
  }
}

/* names */
%inline op:
  | OP { mk_name $1 }
  | R_ANGLE+ { mk_name (String.make (List.length $1) '>') }

lcid: LCID { mk_name $1 }
ucid: UCID { mk_name $1 }

ucid_name:
  | ucid_name DOT ucid { $1 @ [$3] }
  | ucid { [$1] }

lcid_name:
  | ucid_name DOT lcid { $1 @ [$3] }
  | lcid { [$1] }

/* type alias */
type_alias: TYPE ucid generic_parameters EQ type_ {
  TypeAlias {
    ta_name = $2;
    ta_generics = $3;
    ta_type = $5;
  }
}

/* imports */
import:
  | GLOBAL IMPORT ucid_name import_items {{
    i_loc = mk_loc();
    i_global = true;
    i_module = $3;
    i_alias = None;
    i_items = $4;
  }}
  | IMPORT ucid_name alias? import_items {{
    i_loc = mk_loc();
    i_global = false;
    i_module = $2;
    i_alias = $3;
    i_items = $4;
  }}

alias: AS ucid { $2 }

import_items:
  | %prec BELOW_PAREN { None }
  | blist(import_item) { Some $1 }

import_item:
  | lcid { ImportValue $1 }
  | ucid plist(ucid)? { ImportType ($1, $2) }

/* if */
if_: IF expr braces(list(stmt)) option(else_) {{
  if_cond = $2;
  if_conseq = $3;
  if_alt = $4;
}}

else_: ELSE else_value { $2 }

else_value:
  | if_ { ElseIf $1 }
  | braces(list(stmt)) { ElseBlock $1 }

/* classes */

class_: CLASS ucid generic_parameters braces(class_body) {
  Class {
    class_name = $2;
    class_generics = $3;
    class_props = fst $4;
    class_fns = snd $4;
  }
}

class_body: class_prop* function_* { ($1, $2) }

class_prop: LET lcid COLON type_ {{
  cp_name = $2;
  cp_type = $4;
}}

%inline class_constructor_no_gen: ucid_name plist(record_field) {
  ClassCtor {
    cc_name = $1;
    cc_generics = [];
    cc_record = $2;
  }
}

%inline class_constructor_full: ucid_name generic_arguments_strict plist(record_field) {
  ClassCtor {
    cc_name = $1;
    cc_generics = $2;
    cc_record = $3;
  }
}

%inline class_constructor:
  | class_constructor_no_gen { $1 }
  | class_constructor_full { $1 }

%inline method_call: atom DOT lcid plist(expr) {
  MethodCall {
    mc_object = $1;
    mc_method = $3;
    mc_args = $4;
    mc_ty_args = [];
  }
}
