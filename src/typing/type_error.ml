open Types
open Fmt

module A = Absyn

type error =
  | Unification_error of texpr * texpr
  | Instance_not_found of texpr * interface_desc
  | Unknown_type of A.qualified_name
  | Unknown_ctor of A.qualified_name
  | Unknown_value of A.qualified_name
  | Unknown_module of A.name
  | Invalid_constraint of A.name * texpr
  | Value_as_type of texpr
  | Invalid_generic_application
  | Invalid_generic_application_few
  | Invalid_application
  | Invalid_implementation of A.qualified_name * texpr
  | Invalid_implementation_type of A.qualified_name * texpr
  | Unknown_field of A.name * texpr
  | Invalid_access of A.name * texpr
  | Invalid_pattern of A.pattern * texpr
  | Precedence_error of A.name * A.name
  | Extraneous_implementation of string * A.name
  | Missing_implementation of string * string
  | Unknown_property of A.name
  | Unknown_method of A.name * string
  | Missing_properties of string list
  | Constructor_not_class of A.qualified_name
  | Callee_not_object of texpr * A.name

exception Error of error

let report_error' ppf = function
  | Unification_error (t1, t2) ->
    pf ppf "Failed to unify %a with %a"
      Printer.Type.pp t1 Printer.Type.pp t2
  | Instance_not_found (t, intf) ->
    pf ppf "No instance of %s found for type %a"
      intf.intf_name Printer.Type.pp t
  | Unknown_type name ->
    pf ppf "Unknown type: %a" Printer.Absyn.pp_qualified_name name
  | Unknown_ctor name ->
    pf ppf "Unknown constructor: %a" Printer.Absyn.pp_qualified_name name
  | Unknown_value name ->
    pf ppf "Unknown variable: %a" Printer.Absyn.pp_qualified_name name
  | Unknown_module name ->
    pf ppf "Unknown module: %s" name.A.str
  | Invalid_constraint (name, ty) ->
    pf ppf "Invalid constraint on generic type %s: expected an interface but found %a"
      name.A.str Printer.Type.pp ty
  | Value_as_type t ->
    pf ppf "Expected a Type, but found %a"
      Printer.Type.pp t
  | Invalid_generic_application ->
    pf ppf "Invalid generic application: applied to too many types"
  | Invalid_generic_application_few ->
    pf ppf "Invalid generic application: not applied to enough arguments"
  | Invalid_application ->
    pf ppf "Invalid application: applied to too many arguments"
  | Invalid_implementation (name, t) ->
    pf ppf "Trying to implement %a for %a, but %a is not an interface"
      Printer.Absyn.pp_qualified_name name
      Printer.Type.pp t
      Printer.Absyn.pp_qualified_name name
  | Invalid_implementation_type (name, t) ->
    pf ppf "Trying to implement %a for %a, but %a should be a simple type, class or enum (monomorphic)"
      Printer.Absyn.pp_qualified_name name
      Printer.Type.pp t
      Printer.Type.pp t
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
  | Extraneous_implementation (intf_name, fn_name) ->
    pf ppf "Implementing function %a but it's not part of the interface %s"
      Printer.pp_name fn_name
      intf_name
  | Missing_implementation (intf_name, fn_name) ->
    pf ppf "Function %s missing from implementation of %s" fn_name intf_name
  | Unknown_property p ->
    pf ppf "Unknown property: %a" Printer.pp_name p
  | Unknown_method (m, c) ->
    pf ppf "Trying to access unknown method %a in object of type %s" Printer.pp_name m c
  | Missing_properties ps ->
    pf ppf "Missing properties: %a" (Printer.comma_sep string) ps
  | Constructor_not_class c ->
    pf ppf "Trying to instantiate %a, but it's not a class" Printer.Absyn.pp_qualified_name c
  | Callee_not_object (obj, method_)  ->
    pf ppf "Trying to call method %a of non-object of type %a" Printer.pp_name method_ Printer.Type.pp obj

let report_error ppf err =
  report_error' ppf err;
  Format.pp_print_newline ppf ()
