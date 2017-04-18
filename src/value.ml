module A = Absyn

type value =
  | Unit
  | Literal of Absyn.literal
  | Ctor of Absyn.ctor
  | Type of string
  | Function of Absyn.function_

let expr_of_value = function
  | Unit -> A.Unit
  | Literal l -> A.Literal l
  | Ctor c -> A.Ctor c
  | Function f -> A.Function f
  | Type _ -> assert false (* can't be converted *)
