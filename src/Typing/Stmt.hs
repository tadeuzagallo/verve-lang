module Typing.Stmt
  ( i_stmts
  ) where

import Typing.Env
import Typing.Decl
import Typing.Expr
import Typing.Types

import Absyn.Base
import Absyn.Meta
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

i_stmts :: [U.Stmt] -> Tc ([T.Stmt], Maybe Type)
i_stmts [] =
  return ([], Nothing)

i_stmts (meta :< Decl decl : stmts) = do
  decl' <- c_decl decl
  continue (meta :< Decl decl') stmts

i_stmts (meta :< Expr expr : stmts) = do
  (expr', ty) <- i_expr expr
  if null stmts
     then return ([meta :< Expr expr'], Just ty)
     else continue (meta :< Expr expr') stmts

continue :: T.Stmt -> [U.Stmt] -> Tc ([T.Stmt], Maybe Type)
continue stmt stmts = do
  (stmts', ty) <- i_stmts stmts
  return (stmt : stmts', ty)
