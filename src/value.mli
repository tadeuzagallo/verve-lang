module A = Absyn

type t =
  | Unit
  | Literal of A.literal
  | Ctor of t A.ctor
  | Type of string
  | Function of A.function_
  | InterfaceFunction of string
  | Record of (string * t) list
  | Builtin of string * builtin

and builtin = (string * t) list -> t list -> (t * (string * t) list)

val expr_of_value : t -> A.expr
