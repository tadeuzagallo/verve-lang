open Types
open Fmt

module A = Absyn

type error =
  | Unification_error of texpr * texpr
  | Instance_not_found of texpr * interface_desc
  | Unknown_type of A.name
  | Unknown_ctor of A.name
  | Unknown_value of A.name
  | Invalid_constraint of A.name * texpr
  | Value_as_type of texpr
  | Invalid_generic_application
  | Invalid_application
  | Invalid_implementation of A.name * texpr
  | Unknown_field of A.name * texpr
  | Invalid_access of A.name * texpr
  | Invalid_pattern of A.pattern * texpr
  | Precedence_error of A.name * A.name

exception Error of error

let report_error' ppf = function
  | Unification_error (t1, t2) ->
    pf ppf "Failed to unify %a with %a"
      Printer.Type.pp t1 Printer.Type.pp t2
  | Instance_not_found (t, intf) ->
    pf ppf "No instance of %s found for type %a"
      intf.intf_name Printer.Type.pp t
  | Unknown_type name ->
    pf ppf "Unknown type: %s" name.A.str
  | Unknown_ctor name ->
    pf ppf "Unknown constructor: %s" name.A.str
  | Unknown_value name ->
    pf ppf "Unknown variable: %s" name.A.str
  | Invalid_constraint (name, ty) ->
    pf ppf "Invalid constraint on generic type %s: expected an interface but found %a"
      name.A.str Printer.Type.pp ty
  | Value_as_type t ->
    pf ppf "Expected a Type, but found %a"
      Printer.Type.pp t
  | Invalid_generic_application ->
    pf ppf "Invalid generic application: applied to too many types"
  | Invalid_application ->
    pf ppf "Invalid application: applied to too many arguments"
  | Invalid_implementation (name, t) ->
    pf ppf "Trying to implement %s for %a, but %s is not an interface"
      name.A.str Printer.Type.pp t name.A.str
  | Unknown_field (field, record) ->
    pf ppf "Trying to access unknown property %s of object of type %a"
      field.A.str Printer.Type.pp record
  | Invalid_access (field, t) ->
    pf ppf "Not an object: Trying to access field %s of value of type %a"
      field.A.str Printer.Type.pp t
  | Invalid_pattern (pat, ty) ->
    pf ppf "Invalid pattern: Trying to use object of type %a as pattern %a"
      Printer.Type.pp ty
      Printer.Absyn.pp_pattern pat
  | Precedence_error (op1, op2) ->
    pf ppf "Precedence error: cannot mix %s and %s in the same infix expression" op1.A.str op2.A.str

let report_error ppf err =
  report_error' ppf err;
  Format.pp_print_newline ppf ()
