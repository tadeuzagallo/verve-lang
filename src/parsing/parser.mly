%{

open Absyn
%}

/* keywords */
%token ENUM
%token FN
%token INTERFACE
%token IMPLEMENTATION

/* punctuation */
%token ARROW
%token COLON
%token COMMA
%token L_ANGLE
%token R_ANGLE
%token L_BRACE
%token R_BRACE
%token L_PAREN
%token R_PAREN
%token EOL
%token EOF

/* tokens with semantic values */
%token <string> LCID
%token <string> UCID
%token <int> INT

%start <Absyn.program> program

%%

program: EOL* decl* EOF { { imports = []; exports = []; body = $2 } }

decl:
  | enum EOL+ { $1 }
  | interface EOL+ { $1 }
  | implementation EOL+ { $1 }
  | expr EOL+ { Expr $1 }

atom:
  | LCID { Var $1 }
  | literal { Literal $1 }
  | application { $1 }
  | L_PAREN expr R_PAREN { $2 }

expr:
  | function_ { Function $1 }
  | constructor { $1 }
  | atom { $1 }

/* function expressions */
function_:
  FN LCID generic_parameters? parameters return_type body(expr) { { fn_name = Some $2; fn_generics = $3; fn_parameters = $4; fn_return_type = $5; fn_body = $6 } }

generic_parameters:
  L_ANGLE separated_nonempty_list(COMMA, generic_parameter) R_ANGLE { $2 }

generic_parameter:
  UCID bounded_quantification? { { name = $1; constraints = $2 } }

bounded_quantification:
  COLON quantifiers { $2 }

quantifiers:
  | UCID { [$1] }
  | L_PAREN separated_list(COMMA, UCID) R_PAREN { $2 }

parameters:
  L_PAREN separated_list(COMMA, parameter) R_PAREN { $2 }

parameter:
  pattern COLON type_ { { param_name = $1; param_type = $3 } }

pattern:
  LCID { $1 }

return_type:
  ARROW type_ { $2 }

body(expr):
  L_BRACE EOL* body_(expr) R_BRACE { $3 }

body_(expr):
  | expr { [$1] }
  | body_eol(expr)* { $1 }

body_eol(expr):
  expr EOL+ { $1 }

/* types */
type_:
  | UCID { Con $1 }
  | UCID generic_arguments { Inst ($1, $2) }
  | arrow_type { $1 }

arrow_type:
  L_PAREN separated_list(COMMA, type_) R_PAREN ARROW type_ { Arrow($2, $5) }

/* application */

application:
  atom generic_arguments? arguments { Application { callee = $1; generic_arguments = $2; arguments = $3; impl_type=None } }

generic_arguments:
  L_ANGLE separated_nonempty_list(COMMA, type_) R_ANGLE { $2 }

arguments:
  L_PAREN separated_list(COMMA, expr) R_PAREN { $2 }

/* literals */
literal:
  | int_ { $1 }

int_:
  INT { Int $1 }

/* enums */
enum:
  ENUM UCID generic_parameters? body(enum_item) { Enum { enum_name = $2; enum_generics = $3; enum_items = $4 } }

enum_item:
  UCID generic_parameters? enum_item_type? { { enum_item_name = $1; enum_item_generics = $2; enum_item_parameters = $3; } }

enum_item_type:
  L_PAREN separated_nonempty_list(COMMA, type_) R_PAREN { $2 }

constructor:
  UCID generic_arguments? arguments? { Ctor { ctor_name = $1; ctor_generic_arguments = $2; ctor_arguments = $3 } }

/* interfaces */
interface:
  INTERFACE UCID L_ANGLE generic_parameter R_ANGLE body(prototype) { Interface { intf_name = $2; intf_param = $4; intf_functions = $6; } }

prototype:
  FN LCID generic_parameters? proto_params return_type { { proto_name = $2; proto_generics = $3; proto_params = $4; proto_ret_type = $5 } }

proto_params:
  L_PAREN separated_list(COMMA, type_) R_PAREN { $2 }

/* implementations */
implementation:
  IMPLEMENTATION UCID L_ANGLE type_ R_ANGLE body(function_) { Implementation { impl_name = $2; impl_arg = $4; impl_functions = $6; impl_arg_type = None } }
