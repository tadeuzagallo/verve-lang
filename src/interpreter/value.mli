module A = Absyn

type t =
  | Unit
  | Literal of A.literal
  | Ctor of t A.ctor
  | Type of string
  | Function of A.function_
  | InterfaceFunction of string * Types.texpr option
  | Record of (string * t) list
  | Builtin of string * builtin
  | Class of string * (string * A.function_) list
  | Object of string * (string * A.function_) list * (string * t) list

and builtin = (string * t) list -> t list -> t

val expr_of_value : t -> A.expr
