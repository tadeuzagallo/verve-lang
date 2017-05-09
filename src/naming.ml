open Absyn
open Type_error

type precedence = {
  op : name;
  precedence : int;
  associativity : associativity;
}

and associativity =
  | Right
  | Left
  | None

let default_prec op = {
  op;
  precedence = 512;
  associativity = Left;
}

let extend_env env (k, v) = (k.str, v) :: env
let find name env = List.assoc name.str env

let prec_of_attributes env op attributes =
  let aux prec = function
    (* assoc *)
    | { attr_name = { str = "assoc" }; attr_value = Some (Attribute { attr_name = { str = "left" }}) } ->
      { prec with associativity = Left }
    | { attr_name = { str = "assoc" }; attr_value = Some (Attribute { attr_name = { str = "right" }}) } ->
      { prec with associativity = Right }
    | { attr_name = { str = "assoc" }; attr_value = Some (Attribute { attr_name = { str = "none" }}) } ->
      { prec with associativity = None }

    (* prec *)
    | { attr_name = { str = "prec" }; attr_value = Some (AttrInt i) } ->
      { prec with precedence = i }
    | { attr_name = { str = "prec" }; attr_value = Some (Attribute { attr_name = { str = "higher" }; attr_value = Some (AttrOp op) }) } ->
      (* TODO: check exists *)
      (* TODO: check self reference *)
      { prec with precedence = (find op env).precedence + 1 }
    | { attr_name = { str = "prec" }; attr_value = Some (Attribute { attr_name = { str = "lower" }; attr_value = Some (AttrOp op) }) } ->
      { prec with precedence = (find op env).precedence - 1 }
    | { attr_name = { str = "prec" }; attr_value = Some (Attribute { attr_name = { str = "equal" }; attr_value = Some (AttrOp op) }) } ->
      { prec with precedence = (find op env).precedence }

    | { attr_name } ->
      Printf.fprintf stderr "warning: Unknown attribute: %s\n" attr_name.str;
      flush stderr;
      prec
  in
  List.fold_left aux (default_prec op) attributes

let compare_prec p1 p2 =
  let c = compare p1.precedence p2.precedence in
  if c > 0 then
    `Left
  else if c < 0 then
    `Right
  else
    match p1.associativity, p2.associativity with
    | Left, Left -> `Left
    | Right, Right  -> `Right
    | _ -> raise (Error (Precedence_error (p1.op, p2.op)))

let rec naming_expr env expr =
  let expr_desc = match expr.expr_desc with
  | Binop ({
        bin_lhs={ expr_loc; expr_desc = Binop ({ bin_lhs = b1l; bin_op = b1op; bin_rhs = b1r; } as b1) };
        bin_op = b2op;
        bin_rhs = b2r } as b2) ->
    let b1l' = naming_expr env b1l in
    let b1r' = naming_expr env b1r in
    let b2r' = naming_expr env b2r in
    let p1 = find b1op env in
    let p2 = find b2op env in
    begin match compare_prec p1 p2 with
    | `Left  ->
      Binop { b2 with
              bin_lhs = {
                (* TODO: combine the locations of b1l and b1r *)
                expr_loc = dummy_loc;
                expr_desc = Binop { b1 with bin_lhs = b1l'; bin_rhs = b1r' };
              };
              bin_rhs = b2r'
            }
    | `Right  ->
      Binop { b1 with
              bin_lhs = b1l';
              bin_rhs = {
                (* TODO: combine the locations of b1r and b2r *)
                expr_loc = dummy_loc;
                expr_desc = Binop { b2 with bin_lhs = b1r'; bin_rhs = b2r' };
              }
            }
    end
  | e -> e
  in { expr with expr_desc }

let naming_intf env intf =
  let aux (items, env) = function
    | OperatorPrototype op ->
      let prec = prec_of_attributes env op.oproto_name op.oproto_attributes in
      (OperatorPrototype op :: items), extend_env env (op.oproto_name, prec)
    | Prototype p ->
      (Prototype p :: items), env
  in
  let items', env' = List.fold_left aux ([], env) intf.intf_items in
  { intf with intf_items = List.rev items' }, env'

let naming_stmt env stmt =
  let stmt_desc, env = match stmt.stmt_desc with
  | Expr expr -> Expr (naming_expr env expr), env
  | stmt -> stmt, env
  in { stmt with stmt_desc }, env

let naming_decl env decl =
  let decl_desc, env = match decl.decl_desc with
  | Stmt stmt ->
    let stmt', env' = naming_stmt env stmt in
    Stmt stmt', env'

  | Interface intf ->
    let intf', env' = naming_intf env intf in
    Interface intf', env'

  | Operator op ->
    Operator op, extend_env env (op.op_name, prec_of_attributes env op.op_name op.op_attributes)

  | decl -> decl, env
  in { decl with decl_desc }, env

let naming_decls env decls =
  let aux (decls, env) decl =
    let decl', env' = naming_decl env decl in
    (decl' :: decls), env'
  in
  let decls', env' = List.fold_left aux ([], env) decls in
  List.rev decls', env'

let program program =
  { program with body=fst @@ naming_decls [] program.body }
