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

and builtin = (string * t) list -> t list -> t

let rec expr_of_value value =
  let expr_desc = match value with
  | Unit -> A.Unit
  | Literal l -> A.Literal l
  | Ctor c ->
    let args = match c.A.ctor_arguments with
      | None -> None
      | Some args -> Some (List.map expr_of_value args)
    in A.Ctor { c with A.ctor_arguments = args }
  | Function f -> A.Function f
  | InterfaceFunction (i, Some t) -> A.Var { A.var_name = A.mk_qualified_name [i]; A.var_type = [t] }
  | Record r -> A.Record (List.map (fun (n,v) -> (A.mk_name n, expr_of_value v)) r)
  | Builtin (n, _) ->  A.Var { A.var_name = A.mk_qualified_name [n]; A.var_type = [] }
  | Type _ -> assert false (* can't be converted *)
  in { A.expr_loc = A.dummy_loc; A.expr_desc }
