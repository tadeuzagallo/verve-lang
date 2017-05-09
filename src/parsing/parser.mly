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

%nonassoc BELOW_PAREN
%left L_PAREN L_ANGLE OP

%%

/* Helpers */

%inline angles(x): delimited(L_ANGLE, x, R_ANGLE) { $1 }
%inline braces(x): delimited(L_BRACE, x, R_BRACE) { $1 }
%inline parens(x): delimited(L_PAREN, x, R_PAREN) { $1 }

%inline plist(x): parens(separated_list(COMMA, x)) { $1 }
%inline blist(x): braces(separated_list(COMMA, x)) { $1 }

%inline nonempty_alist(x): angles(separated_nonempty_list(COMMA, x)) { $1 }

/* Entry points */
program: decl* EOF {
  { imports = []; exports = []; body = $1 }
}

decl_start: decl EOF { $1 }

/* high-level structures */
decl: decl_desc { mk_decl $1 }
decl_desc:
  | enum { $1 }
  | interface { $1 }
  | implementation { $1 }
  | type_alias { $1 }
  | operator { Operator $1 }
  | stmt { Stmt $1 }

stmt: stmt_desc { mk_stmt $1 }
stmt_desc:
  | let_ { $1 }
  | function_ { FunctionStmt $1 }
  | expr_ { Expr (mk_expr $1) }

expr: expr_desc { mk_expr $1 }
expr_desc:
  | function_ { Function $1 }
  | expr_ { $1 }

expr_:
  | record { $1 }
  | match_expr { $1 }
  | constructor { $1 }
  | constructor_no_args %prec BELOW_PAREN { $1 }
  | atom_desc %prec BELOW_PAREN { $1 }
  | binop { $1 }

%inline atom: atom_desc { mk_expr $1 }
atom_desc:
  | lcid { Var $1 }
  | literal { Literal $1 }
  | field_access { $1 }
  | application { $1 }
  (* Matched when the first expression in a sequence is wrapped in parens *)
  | parens(expr) { Wrapped $1 }
  | parens(op) { Var $1 }
  (* Matched to break applications when there's a line break *)
  | NL_L_PAREN expr R_PAREN { Wrapped $2 }
  | NL_L_PAREN op R_PAREN { Var $2 }

/* function expressions */
function_: FN lcid generic_parameters plist(parameter) ARROW type_ braces(list(stmt)) {
  { fn_name = Some ($2); fn_generics = $3; fn_parameters = $4; fn_return_type = $6; fn_body = $7 }
}

generic_parameters: loption(nonempty_alist(generic_parameter)) { $1 }

generic_parameter: ucid loption(COLON quantifiers { $2 }) {
  { name = $1; constraints = $2 }
}

quantifiers:
  | ucid { [$1] }
  | plist(ucid) { $1 }

parameter: lcid COLON type_ {
  { param_name = $1; param_type = $3 }
}

/* types */
type_: type_desc { mk_type $1 }

type_desc:
  | ucid generic_arguments { Inst ($1, $2) }
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

enum_item: ucid generic_parameters plist(type_)? {
  { enum_item_name = $1; enum_item_generics = $2; enum_item_parameters = $3; }
}

%inline constructor_no_args: ucid generic_arguments {
  Ctor { ctor_name = $1; ctor_generic_arguments = $2; ctor_arguments = None }
}

%inline constructor: ucid generic_arguments plist(expr) {
  Ctor { ctor_name = $1; ctor_generic_arguments = $2; ctor_arguments = Some $3 }
}

/* interfaces */
interface: INTERFACE ucid angles(generic_parameter) braces(list(interface_item)) {
  Interface { intf_name = $2; intf_param = $3; intf_items = $4; }
}

interface_item:
  | prototype {$1}
  | operator_prototype {$1}

prototype: FN lcid generic_parameters plist(type_) ARROW type_ {
  Prototype {
    proto_name = $2;
    proto_generics = $3;
    proto_params = $4;
    proto_ret_type = $6
  }
}

operator_prototype: attributes OPERATOR generic_parameters parens(type_) op parens(type_) ARROW type_ {
    OperatorPrototype {
      oproto_attributes = $1;
      oproto_generics = $3;
      oproto_lhs = $4;
      oproto_name = $5;
      oproto_rhs = $6;
      oproto_ret_type = $8;
    }
}

/* implementations */
implementation: IMPLEMENTATION ucid angles(type_) braces(list(impl_item)) {
  Implementation { impl_name = $2; impl_arg = $3; impl_items = $4; impl_arg_type = None }
}

impl_item:
  | function_ { ImplFunction $1 }
  | operator { ImplOperator $1 }

/* Records */

record: blist(record_field) { Record $1 }
record_field: lcid EQ expr { ($1, $3) }

field_access: atom DOT lcid {
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
  | ucid option(plist(pattern)) { Pctor ($1, $2) }

/* binary operations */
binop: expr op expr {
  Binop { bin_lhs = $1; bin_op = $2; bin_rhs = $3; bin_generic_arguments_ty = [] }
}

operator: attributes OPERATOR generic_parameters parens(parameter) op parens(parameter) ARROW type_ braces(list(stmt)) {
    {
      op_attributes = $1;
      op_generics = $3;
      op_lhs = $4;
      op_name = $5;
      op_rhs = $6;
      op_ret_type = $8;
      op_body = $9;
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
%inline op: OP { mk_name $1 }
lcid: LCID { mk_name $1 }
ucid: UCID { mk_name $1 }

/* type alias */
type_alias: TYPE ucid EQ type_ {
    TypeAlias ($2, $4)
}
