open Absyn
open Type_error

type precedence = {
  op : string;
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

let extend_env env (k, v) = (k, v) :: env

let prec_of_attributes env op attributes =
  let aux prec = function
    (* assoc *)
    | { attr_name = "assoc"; attr_value = Some (Attribute { attr_name = "left" }) } ->
      { prec with associativity = Left }
    | { attr_name = "assoc"; attr_value = Some (Attribute { attr_name = "right" }) } ->
      { prec with associativity = Right }
    | { attr_name = "assoc"; attr_value = Some (Attribute { attr_name = "none" }) } ->
      { prec with associativity = None }

    (* prec *)
    | { attr_name = "prec"; attr_value = Some (AttrInt i) } ->
      { prec with precedence = i }
    | { attr_name = "prec"; attr_value = Some (Attribute { attr_name = "higher"; attr_value = Some (AttrOp op) }) } ->
      (* TODO: check exists *)
      (* TODO: check self reference *)
      { prec with precedence = (List.assoc op env).precedence + 1 }
    | { attr_name = "prec"; attr_value = Some (Attribute { attr_name = "lower"; attr_value = Some (AttrOp op) }) } ->
      { prec with precedence = (List.assoc op env).precedence - 1 }
    | { attr_name = "prec"; attr_value = Some (Attribute { attr_name = "equal"; attr_value = Some (AttrOp op) }) } ->
      { prec with precedence = (List.assoc op env).precedence }

    | { attr_name } ->
      Printf.fprintf stderr "warning: Unknown attribute: %s\n" attr_name;
      flush stderr;
      prec
  in
  List.fold_left aux (default_prec op) attributes

let compare_prec p1 p2 =
  match compare p1.precedence p2.precedence with
  | x when x > 0 -> `Left
  | x when x < 0 -> `Right
  | 0 -> match p1.associativity, p2.associativity with
         | Left, Left -> `Left
         | Right, Right  -> `Right
         | _ -> raise (Error (Precedence_error (p1.op, p2.op)))

let rec naming_expr env = function
  | Operator op ->
    Operator op, extend_env env (op.op_name, prec_of_attributes env op.op_name op.op_attributes)

  | Binop ({ bin_lhs=Binop ({ bin_lhs = b1l; bin_op = b1op; bin_rhs = b1r; } as b1); bin_op = b2op; bin_rhs = b2r } as b2) ->
    let b1l', _ = naming_expr env b1l in
    let b1r', _ = naming_expr env b1r in
    let b2r', _ = naming_expr env b2r in
    let p1 = List.assoc b1op env in
    let p2 = List.assoc b2op env in
    begin match compare_prec p1 p2 with
    | `Left  ->
      Binop { b2 with bin_lhs = Binop { b1 with bin_lhs = b1l'; bin_rhs = b1r' }; bin_rhs = b2r' }, env
    | `Right  ->
      Binop { b1 with bin_lhs = b1l'; bin_rhs = Binop { b2 with bin_lhs = b1r'; bin_rhs = b2r' } }, env
    end
  | e -> e, env

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

let naming_decl env = function
  | Expr expr ->
    let expr', env' = naming_expr env expr in
    Expr expr', env'
  | Interface intf ->
    let intf', env' = naming_intf env intf in
    Interface intf', env'
  | decl -> decl, env

let naming_decls env decls =
  let aux (decls, env) decl =
    let decl', env' = naming_decl env decl in
    (decl' :: decls), env'
  in
  let decls', env' = List.fold_left aux ([], env) decls in
  List.rev decls', env'

let program program =
  { program with body=fst @@ naming_decls [] program.body }
