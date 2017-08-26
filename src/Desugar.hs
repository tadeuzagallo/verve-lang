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

d_fn :: Function (Id Type) Type -> CA.Expr
d_fn fn@(Function { params=[] }) =
  d_fn (fn { params = [("", void)] })
d_fn fn =
  let fn' = foldr CA.Lam (d_stmts $ body fn) (map (uncurry (,)) $ params fn)
      fn'' = foldr CA.Lam fn' (map (flip (,) Type) $ generics fn)
   in CA.App (CA.Var ("#fix", void)) (CA.Lam (name fn) fn'')

d_expr :: Expr (Id Type) Type -> CA.Expr
d_expr VoidExpr = CA.Void
d_expr (Literal l) = CA.Lit l
d_expr (Ident id) = CA.Var id
d_expr (Call callee types []) = d_expr (Call callee types [VoidExpr])
d_expr (BinOp lhs op rhs) =
  d_expr (Call (Ident op) [] [lhs, rhs])
d_expr (Call callee types args) =
  let app = foldl CA.App (d_expr callee) (CA.Type <$> types)
   in foldl mkApp app args
    where
      mkApp :: CA.Expr -> Expr (Id Type) Type -> CA.Expr
      mkApp callee arg =
        CA.App callee (d_expr arg)

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
