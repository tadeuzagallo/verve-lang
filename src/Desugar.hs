module Desugar
  ( desugar
  , desugarStmt
  ) where

import Absyn
import Types
import qualified CoreAbsyn as CA

desugar :: Module Id -> CA.Expr
desugar = d_stmts . stmts

desugarStmt :: Stmt Id -> CA.Expr
desugarStmt stmt =
  let (x, v) = d_stmt stmt
   in CA.Let [(x, v)] (CA.Var x)

d_stmts :: [Stmt Id] -> CA.Expr
d_stmts [] = CA.Void
d_stmts [s] = snd $ d_stmt s
d_stmts (s:ss) =
  CA.Let [d_stmt s] (d_stmts ss)

d_stmt :: Stmt Id -> CA.Bind
d_stmt (Expr e) = (Id "" void, d_expr e)
d_stmt (FnStmt fn) = (name fn, d_fn fn)
d_stmt (Enum _ _) = (Id "" void, CA.Void)
d_stmt (Operator opLhs opName opRhs opRetType opBody) =
  let opLhs' = Id (fst opLhs) (snd opLhs)
      opRhs' = Id (fst opRhs) (snd opRhs)
   in (opName, CA.Lam opLhs' (CA.Lam opRhs' (d_stmts opBody)))

d_fn :: Function Id -> CA.Expr
d_fn fn@(Function { params=[] }) = d_fn (fn { params = [("", void)] })
d_fn fn =
  let fn' = foldr CA.Lam (d_stmts $ body fn) (map (uncurry Id) $ params fn)
   in foldr CA.Lam fn' (map (flip Id Type) $ generics fn)

d_expr :: Expr Id -> CA.Expr
d_expr VoidExpr = CA.Void
d_expr (Literal l) = CA.Lit l
d_expr (Ident id) = CA.Var id
d_expr (App callee types []) = d_expr (App callee types [VoidExpr])
d_expr (BinOp lhs op rhs) =
  d_expr (App (Ident op) [] [lhs, rhs])
d_expr (App callee types args) =
  let app = foldl CA.App (d_expr callee) (CA.Type <$> types)
   in foldl mkApp app args
    where
      mkApp :: CA.Expr -> Expr Id -> CA.Expr
      mkApp callee arg =
        CA.App callee (d_expr arg)

d_expr (Match expr cases) = CA.Match (d_expr expr) (map d_case cases)

d_case :: Case Id -> CA.Case
d_case (Case pattern expr) = (d_pattern pattern, d_expr expr)

d_pattern :: Pattern Id -> CA.Pattern
d_pattern PatDefault = CA.PatDefault
d_pattern (PatLiteral l) = CA.PatLiteral l
d_pattern (PatVar v) = CA.PatVar v
d_pattern (PatCtor name pats) = CA.PatCtor name (map d_pattern pats)
