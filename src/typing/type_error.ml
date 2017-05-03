open Types
open Fmt

type error =
  | Unification_error of ty * ty
  | Instance_not_found of ty * interface_desc
  | Unknown_type of string
  | Invalid_constraint of string * ty
  | Value_as_type of ty
  | Invalid_generic_application
  | Invalid_application
  | Invalid_implementation of string * ty
  | Unknown_field of string * ty
  | Invalid_access of string * ty
  | Invalid_pattern of Absyn.pattern * ty
  | Precedence_error of string * string

exception Error of error

let report_error ppf = function
  | Unification_error (t1, t2) ->
    pf ppf "Failed to unify %a with %a"
      Printer.Type.pp t1 Printer.Type.pp t2
  | Instance_not_found (t, intf) ->
    pf ppf "No instance of %s found for type %a"
      intf.intf_name Printer.Type.pp t
  | Unknown_type name ->
    pf ppf "Unknown type: %s" name
  | Invalid_constraint (name, ty) ->
    pf ppf "Invalid constraint on generic type %s: expected an interface but found %a"
      name Printer.Type.pp ty
  | Value_as_type t ->
    pf ppf "Expected a Type, but found %a"
      Printer.Type.pp t
  | Invalid_generic_application ->
    pf ppf "Invalid generic application: applied to too many types"
  | Invalid_application ->
    pf ppf "Invalid application: applied to too many arguments"
  | Invalid_implementation (name, t) ->
    pf ppf "Trying to implement %s for %a, but %s is not an interface"
      name Printer.Type.pp t name
  | Unknown_field (field, record) ->
    pf ppf "Trying to access unknown property %s of object of type %a"
      field Printer.Type.pp record
  | Invalid_access (field, t) ->
    pf ppf "Not an object: Trying to access field %s of value of type %a"
      field Printer.Type.pp t
  | Invalid_pattern (pat, ty) ->
    pf ppf "Invalid pattern: Trying to use object of type %a as pattern %a"
      Printer.Type.pp ty
      Printer.Absyn.pp_pattern pat
  | Precedence_error (op1, op2) ->
    pf ppf "Precedence error: cannot mix %s and %s in the same infix expression" op1 op2
