module Typing.Stmt
  ( i_stmts
  ) where

import Typing.Decl
import Typing.Expr
import Typing.State
import Typing.Types

import Absyn.Base
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

i_stmts :: [U.Stmt] -> Tc ([T.Stmt], Maybe Type)
i_stmts [] =
  return ([], Nothing)

i_stmts (Decl decl : stmts) = do
  decl' <- c_decl decl
  continue (Decl decl') stmts

i_stmts (Expr expr : stmts) = do
  (expr', ty) <- i_expr expr
  if null stmts
     then return ([Expr expr'], Just ty)
     else continue (Expr expr') stmts

continue :: T.Stmt -> [U.Stmt] -> Tc ([T.Stmt], Maybe Type)
continue stmt stmts = do
  (stmts', ty) <- i_stmts stmts
  return (stmt : stmts', ty)

