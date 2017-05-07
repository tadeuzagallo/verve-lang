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

let rec expr_of_value = function
  | Unit -> A.Unit
  | Literal l -> A.Literal l
  | Ctor c ->
    let args = match c.A.ctor_arguments with
      | None -> None
      | Some args -> Some (List.map expr_of_value args)
    in A.Ctor { c with A.ctor_arguments = args }
  | Function f -> A.Function f
  | InterfaceFunction i -> A.Var i
  | Record r -> A.Record (List.map (fun (n,v) -> (n, expr_of_value v)) r)
  | Builtin (n, _) -> A.Var n
  | Type _ -> assert false (* can't be converted *)
