open Absyn

module V = Value

let fn_to_intf : (string, string) Hashtbl.t = Hashtbl.create 256
let intf_to_impls : (string, (Types.texpr * (string * function_) list) list ref) Hashtbl.t = Hashtbl.create 64

let int_op op env = function
  | [V.Literal (Int x); V.Literal (Int y)] ->
    V.Literal (Int (op x y))
  | _ -> assert false

let extend_env env name value =
  (name, value) :: env

let extend_name env name value =
  extend_env env name.str value

let last name = List.nth name (List.length name - 1)

let find_name name env =
  List.assoc (last name).str env

let add_builtin name fn env =
  extend_env env name (V.Builtin (name, fn))

let default_env = []
  |> add_builtin "int_add" (int_op ( + ))
  |> add_builtin "int_sub" (int_op ( - ))
  |> add_builtin "int_mul" (int_op ( * ))
  |> add_builtin "int_div" (int_op ( / ))
