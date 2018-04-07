module Reassoc.Reassoc
  ( reassocStmts
  ) where

import Absyn.Untyped
import Reassoc.Env
import Reassoc.Error
import Util.Error

import Control.Monad (foldM)

reassocStmts :: Env -> [Stmt] -> Result (Env, [Stmt])
reassocStmts = n_stmts

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
n_decl env enum@(Enum {}) = return (env, enum)
n_decl env ta@(TypeAlias {}) = return (env, ta)
n_decl env impl@(Implementation { implMethods }) = do
  implMethods' <- mapM (n_implItem env) implMethods
  return (env, impl { implMethods = implMethods' })

n_intfItem :: Env -> InterfaceItem -> Result Env
n_intfItem env (IntfOperator { intfOpName, intfOpPrec, intfOpAssoc }) = do
  intfOpPrec' <- prec env intfOpPrec
  let env' = addOpInfo env (intfOpName, (intfOpAssoc, intfOpPrec'))
  return env'

n_intfItem env _ =
  return env

n_implItem :: Env -> ImplementationItem -> Result ImplementationItem
n_implItem env (ImplVar (a, e)) = do
  e' <- n_expr env e
  return $ ImplVar (a, e')

n_implItem env fn@(ImplFunction { implBody }) = do
  implBody' <- snd <$> n_stmts env implBody
  return $ fn { implBody = implBody' }

n_implItem env op@(ImplOperator { implOpBody }) = do
  implOpBody' <- snd <$> n_stmts env implOpBody
  return $ op { implOpBody = implOpBody' }

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

n_expr _ (Literal l) = return $ Literal l
n_expr _ (Ident a b) = return $ Ident a b

n_expr env (ParenthesizedExpr e) =
  ParenthesizedExpr <$> n_expr env e

n_expr env (Match e cs) = do
  e' <- n_expr env e
  cs' <- mapM (n_case env) cs
  return $ Match e' cs'

n_expr env (If cond conseq alt) = do
  cond' <- n_expr env cond
  conseq' <- snd <$> n_stmts env conseq
  alt' <- snd <$> n_stmts env alt
  return $ If cond' conseq' alt'

n_expr env call@(Call { callee, args }) = do
  callee' <- n_expr env callee
  args' <- mapM (n_expr env) args
  return $ call { callee = callee', args = args' }

n_expr env op@(BinOp { lhs, rhs }) = do
  lhs' <- n_expr env lhs
  rhs' <- n_expr env rhs
  return $ op { lhs = lhs', rhs = rhs' }

n_expr env (Record fields) = do
  fields' <- mapM (mapM $ n_expr env) fields
  return $ Record fields'

n_expr env (List t items) = do
  items' <- mapM (n_expr env) items
  return $ List t items'

n_expr env (FieldAccess obj t field) = do
  obj' <- n_expr env obj
  return $ FieldAccess obj' t field

n_expr env (FnExpr fn) =
  FnExpr <$> n_fn env fn

n_expr env (Negate cArgs e) = do
  e' <- n_expr env e
  return $ Negate cArgs e'

n_expr _ VoidExpr = return VoidExpr

n_expr env (TypeCall e cArgs) = do
  e' <- n_expr env e
  return $ TypeCall e' cArgs

n_case :: Env -> Case -> Result Case
n_case env (Case pattern body) = do
  body' <- snd <$> n_stmts env body
  return $ Case pattern body'

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
