module Typing.Stmt
  ( i_stmts
  , i_stmt
  ) where

import Typing.Ctx
import Typing.Decl
import Typing.Expr
import Typing.State
import Typing.Types

import Absyn.Base
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Control.Monad (foldM)

i_stmts :: Ctx -> [U.Stmt] -> Tc (Ctx, [T.Stmt], Type)
i_stmts ctx stmts = do
  (ctx, stmts', ty) <- foldM aux (ctx, [], void) stmts
  return (ctx, reverse stmts', ty)
    where
      aux :: (Ctx, [T.Stmt], Type) -> U.Stmt -> Tc (Ctx, [T.Stmt], Type)
      aux (ctx, stmts, _) stmt = do
        (ctx', stmt', ty) <- i_stmt ctx stmt
        return (ctx', stmt':stmts, ty)

i_stmt :: Ctx -> U.Stmt -> Tc (Ctx, T.Stmt, Type)
i_stmt ctx (Expr expr) = do
  (expr', ty) <- i_expr ctx expr
  return (ctx, Expr expr', ty)
i_stmt ctx (Decl decl) = do
  (ctx', decl', ty) <- i_decl ctx decl
  return (ctx', Decl decl', ty)
