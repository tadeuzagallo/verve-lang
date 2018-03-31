module Typing.Stmt
  ( i_stmts
  ) where

import Typing.Ctx
import Typing.Decl
import Typing.Expr
import Typing.State
import Typing.Types

import Absyn.Base
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

i_stmts :: Ctx -> [U.Stmt] -> Tc (Ctx, [T.Stmt], Maybe Type)
i_stmts ctx [] =
  return (ctx, [], Nothing)

i_stmts ctx (Decl decl : stmts) = do
  (ctx', decl', _) <- i_decl ctx decl
  continue ctx' (Decl decl') stmts

i_stmts ctx (Expr expr : stmts) = do
  (expr', ty) <- i_expr ctx expr
  if null stmts
     then return (ctx, [Expr expr'], Just ty)
     else do continue ctx (Expr expr') stmts

continue :: Ctx -> T.Stmt -> [U.Stmt] -> Tc (Ctx, [T.Stmt], Maybe Type)
continue ctx stmt stmts = do
  (ctx', stmts', ty) <- i_stmts ctx stmts
  return (ctx', stmt : stmts', ty)

