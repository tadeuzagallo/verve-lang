module Desugar
  ( desugar
  , desugarStmt
  ) where

import Absyn
import Types
import qualified CoreAbsyn as CA

desugar :: Module (Id Type) Type -> CA.Expr
desugar = d_stmts . stmts

desugarStmt :: Stmt (Id Type) Type -> CA.Expr
desugarStmt stmt = d_stmts [stmt]

d_stmts :: [Stmt (Id Type) Type] -> CA.Expr
d_stmts [] = CA.Void
-- terminators
d_stmts ([Expr e]) =
  d_expr e
d_stmts ([FnStmt fn]) =
  let fn' = d_fn fn
   in CA.Let [(name fn, fn')] (CA.Var $ name fn)

-- intermediaries
d_stmts (Let var expr : ss) =
  CA.Let [(var, d_expr expr)] (d_stmts ss)
d_stmts (Expr e:ss) =
  CA.Let [(("", void), d_expr e)] (d_stmts ss)
d_stmts (FnStmt fn:ss) =
  CA.Let [(name fn, d_fn fn)] (d_stmts ss)
d_stmts (Enum name _ _ : ss) =
  CA.Let [(("", Type), CA.Var name)] (d_stmts ss)
d_stmts (Operator _ _ opGenerics opLhs opName opRhs opRetType opBody : ss) =
  let fn = d_fn (Function { name = opName
                          , generics = opGenerics
                          , params = [opLhs, opRhs]
                          , retType = opRetType
                          , body = opBody
                          })
   in CA.Let [(opName, fn)] (d_stmts ss)
d_stmts (Class _ _ methods : ss) =
  let methods' = map (\fn -> (name fn, d_fn fn)) methods
   in CA.Let methods' (d_stmts ss)

d_stmts (Interface _ _ methods : ss) =
  CA.Let (map d_intfMethod methods) (d_stmts ss)

d_stmts (Implementation (name, _) ty methods : ss) =
  let dict = CA.Record (map d_implMethod methods)
   in CA.Let [(("#" ++ name ++ show ty, void), dict)] (d_stmts ss)

d_fn :: Function (Id Type) Type -> CA.Expr
d_fn fn@(Function { params=[] }) =
  d_fn (fn { params = [("", void)] })
d_fn fn =
  let fn' = foldr CA.Lam (d_stmts $ body fn) (map (uncurry (,)) $ params fn)
      fn'' = foldr CA.Lam fn' (map (flip (,) Type . fst) $ generics fn)
      fn''' = foldr CA.Lam fn'' (concatMap constraints $ generics fn)
   in CA.App (CA.Var ("#fix", void)) (CA.Lam (name fn) fn''')
  where
    constraints (varName, bounds) =
      map (\bound -> ("#" ++ show bound ++ varName, void)) bounds

mk_var :: String -> CA.Expr
mk_var v = CA.Var (v, void)

d_intfMethod :: FunctionDecl (Id Type) Type -> CA.Bind
d_intfMethod (FunctionDecl name@(s_name, _) _ _ _) =
  let select = CA.App (mk_var "#fieldAccess") (CA.Lit (String s_name))
      select' = CA.App select (mk_var "#dict")
   in (name, CA.Lam ("#dict", void) (CA.Lam ("", Type) select'))

d_implMethod :: Function (Id Type) Type -> CA.Bind
d_implMethod fn = (name fn, d_fn fn)

d_expr :: Expr (Id Type) Type -> CA.Expr
d_expr VoidExpr = CA.Void
d_expr (Literal l) = CA.Lit l
d_expr (Ident id) = CA.Var id
d_expr (Call callee constraints types []) = d_expr (Call callee constraints types [VoidExpr])
d_expr (BinOp tyArgs lhs op rhs) =
  -- TODO: missing constraintArgs
  d_expr (Call (Ident op) [] tyArgs [lhs, rhs])
d_expr (Call callee constraints types args) =
  let app = foldl CA.App (d_expr callee) (map mkConstraint constraints)
      app' = foldl CA.App app (CA.Type <$> types)
   in foldl mkApp app' args
    where
      mkApp :: CA.Expr -> Expr (Id Type) Type -> CA.Expr
      mkApp callee arg =
        CA.App callee (d_expr arg)

      mkConstraint (typeArg, typeBound) =
        mk_var ("#" ++ show typeBound ++ show typeArg)

d_expr (Match expr cases) = CA.Match (d_expr expr) (map d_case cases)

d_expr (Record fields) =
  let fields' = map (\(a, b) -> (a, d_expr b)) fields
   in CA.Record fields'

d_expr (FieldAccess expr ty (field, _)) =
  let dexpr = (d_expr expr)
      expr' =
        case ty of
          Cls _ _ -> CA.App (CA.Var ("#unwrapClass", void)) dexpr
          Rec _ -> dexpr
          _ -> undefined
   in CA.App
    (CA.App (CA.Var ("#fieldAccess", void)) (CA.Lit $ String field))
    expr'

d_expr (If ifCond ifBody elseBody) =
  CA.Match (d_expr ifCond) [ (CA.PatCtor ("True", bool) [], d_stmts ifBody)
                           , (CA.PatCtor ("False", bool) [], d_stmts elseBody)
                           ]

d_expr (List items) =
  aux items
    where
      aux [] = nil
      aux (x:xs) = cons (d_expr x) (aux xs)
      nil = CA.Var ("Nil", void)
      cons head tail = CA.App (CA.App (CA.Var ("Cons", void)) head) tail

d_case :: Case (Id Type) Type -> CA.Case
d_case (Case pattern expr) = (d_pattern pattern, d_stmts expr)

d_pattern :: Pattern (Id Type) -> CA.Pattern
d_pattern PatDefault = CA.PatDefault
d_pattern (PatLiteral l) = CA.PatLiteral l
d_pattern (PatVar v) = CA.PatVar v
d_pattern (PatCtor name pats) = CA.PatCtor name (map d_pattern pats)
