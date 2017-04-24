module A = Absyn

type value =
  | Unit
  | Literal of Absyn.literal
  | Ctor of Absyn.ctor
  | Type of string
  | Function of Absyn.function_
  | InterfaceFunction of string

let expr_of_value = function
  | Unit -> A.Unit
  | Literal l -> A.Literal l
  | Ctor c -> A.Ctor c
  | Function f -> A.Function f
  | InterfaceFunction i -> A.Var i
  | Type _ -> assert false (* can't be converted *)
