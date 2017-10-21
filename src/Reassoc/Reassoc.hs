module Reassoc.Reassoc
  ( reassoc
  , reassocStmt
  ) where

import Absyn.Untyped
import Reassoc.Env
import Reassoc.Error
import Util.Error

import Control.Monad (foldM)

reassoc :: Module -> Result Module
reassoc (Module imports stmts) = do
  (_, stmts') <- n_stmts defaultEnv stmts
  return $ Module imports stmts'

reassocStmt :: Env -> Stmt -> Result (Env, Stmt)
reassocStmt = n_stmt

n_stmts :: Env -> [Stmt] -> Result (Env, [Stmt])
n_stmts env stmts = do
  (env', stmts') <- foldM aux (env, []) stmts
  return (env', reverse stmts')
    where
      aux (env, stmts) stmt = do
        (env', stmt') <- n_stmt env stmt
        return (env', stmt' : stmts)

n_stmt :: Env -> Stmt -> Result (Env, Stmt)
n_stmt env (Expr expr) = do
  expr' <- n_expr env expr
  return (env, Expr expr')
n_stmt env (Decl decl) = do
  (env', decl') <- n_decl env decl
  return (env', Decl decl')

-- TODO: Remove catch all case
n_decl :: Env -> Decl -> Result (Env, Decl)
n_decl env op@(Operator { opAssoc, opPrec, opName, opBody }) = do
  opPrec' <- prec env opPrec
  let env' = addOpInfo env (opName, (opAssoc, opPrec'))
  (_, opBody') <- n_stmts env' opBody
  return (env', op { opBody = opBody' })
n_decl env (FnStmt fn) = do
  fn' <- n_fn env fn
  return (env, FnStmt fn')
n_decl env (Let x expr) = do
  expr' <- n_expr env expr
  return (env, Let x expr')
n_decl env (Class name vars methods) = do
  methods' <- mapM (n_fn env) methods
  return (env, Class name vars methods')
n_decl env intf@(Interface _ _ methods) = do
  env' <- foldM n_intfItem env methods
  return (env', intf)
n_decl env stmt = return (env, stmt)

n_intfItem :: Env -> InterfaceItem -> Result Env
n_intfItem env (IntfOperator { intfOpName, intfOpPrec, intfOpAssoc }) = do
  intfOpPrec' <- prec env intfOpPrec
  let env' = addOpInfo env (intfOpName, (intfOpAssoc, intfOpPrec'))
  return env'

n_intfItem env _ =
  return env


prec :: Env -> Precedence -> Result PrecInt
prec _ (PrecValue n) = return n
prec env (PrecEqual n) =  getPrec n env
prec env (PrecHigher n) =  (+) 1 <$> getPrec n env
prec env (PrecLower n) =  (-) 1 <$> getPrec n env

n_fn :: Env -> Function -> Result Function
n_fn env fn = do
  (_, body') <- n_stmts env (body fn)
  return fn { body = body' }

n_expr :: Env -> Expr -> Result Expr
n_expr env (BinOp _ _ ll lop (BinOp _ _ rl rop rr)) = do
  ll' <- n_expr env ll
  rl' <- n_expr env rl
  rr' <- n_expr env rr
  c <- comparePrec env lop rop
  return $ case c of
    PLeft ->
      BinOp [] [] (BinOp [] [] ll' lop rl') rop rr'
    PRight ->
      BinOp [] [] ll' lop (BinOp [] [] rl' rop rr')

-- TODO: this should propagate, otherwise nested operations won't be re-associated
-- COUNTER-EXAMPLE: (fn f() -> Int { 2 * 3 + 4 })()
n_expr _ expr = return expr

data Prec
  = PLeft
  | PRight

comparePrec :: Env -> Name -> Name -> Result Prec
comparePrec env l r = do
  (lAssoc, lPrec) <- getOpInfo l env
  (rAssoc, rPrec) <- getOpInfo r env
  case (compare lPrec rPrec, lAssoc, rAssoc) of
    (LT, _, _) -> return PRight
    (GT, _, _) -> return PLeft
    (EQ, AssocLeft, AssocLeft) -> return PLeft
    (EQ, AssocRight, AssocRight) -> return PRight
    (EQ, _, _) -> mkError $ PrecedenceError l r
