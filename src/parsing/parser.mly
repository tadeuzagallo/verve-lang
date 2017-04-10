%{

open Absyn

%}

/* keywords */
%token FN

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
%token EOF

/* tokens with semantic values */
%token <string> LCID
%token <string> UCID

%start <Absyn.expr list> program

%%

program: exprs EOF { $1 }

exprs:
  expr* { $1 }

expr:
  function_ { $1 }

/* function expressions */
function_:
  FN LCID generic_parameters parameters return_type function_body { E ($2, $3, $4, $5, $6) }

generic_parameters:
  L_ANGLE separated_list(COMMA, generic_parameter) R_ANGLE { $2 }

generic_parameter:
  UCID bounded_quantification? { ($1, $2) }

bounded_quantification:
  COLON quantifiers { $2 }

quantifiers:
  | UCID { [$1] }
  | L_PAREN separated_list(COMMA, UCID) R_PAREN { $2 }

parameters:
  L_PAREN separated_list(COMMA, parameter) R_PAREN { $2 }

parameter:
  pattern COLON type_ { ($1, $3) }

pattern:
  LCID { $1 }

return_type:
  ARROW type_ { $2 }

function_body:
  L_BRACE exprs R_BRACE { $2 }

/* types */
type_:
  UCID { $1 }
