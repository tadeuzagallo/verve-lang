module Reassoc.Reassoc
  ( reassocStmts
  ) where

import Reassoc.Env
import Reassoc.Error

import Absyn.Untyped
import Util.Error
import Util.Scope (runScoped)

reassocStmts :: [Stmt] -> ReassocEnv -> Result (ReassocEnv, [Stmt])
reassocStmts stmts state =
  runScoped (n_stmts stmts) state

wrapFn :: (ASTNode node name () -> Reassoc (ASTNode node name ())) -> AST node name () -> Reassoc (AST node name ())
wrapFn f (() :< e) =
  (() :<) <$> f e

n_stmts :: [Stmt] -> Reassoc [Stmt]
n_stmts = mapM n_stmt

n_stmt :: Stmt -> Reassoc Stmt
n_stmt = wrapFn n_stmt
  where
    n_stmt (Expr expr) = do
      Expr <$> n_expr expr
    n_stmt (Decl decl) = do
      Decl <$> n_decl decl

n_codeBlock :: CodeBlock -> Reassoc CodeBlock
n_codeBlock (() :< CodeBlock stmts) =
  (() :<) . CodeBlock <$> n_stmts stmts

n_decl :: Decl -> Reassoc Decl
n_decl = wrapFn n_decl
  where
    n_decl op@(Operator { opAssoc, opPrec, opName, opBody }) = do
      opPrec' <- prec opPrec
      addOpInfo opName (opAssoc, opPrec')
      opBody' <- n_codeBlock opBody
      return op { opBody = opBody' }
    n_decl (FnStmt fn) = do
      FnStmt <$> n_fn fn
    n_decl (Let x expr) = do
      expr' <- n_expr expr
      return $ Let x expr'
    n_decl (Class name vars methods) = do
      methods' <- mapM n_fn methods
      return $ Class name vars methods'
    n_decl intf@(Interface _ _ methods) = do
      mapM_ n_intfItem methods
      return intf
    n_decl enum@(Enum {}) = return enum
    n_decl ta@(TypeAlias {}) = return ta
    n_decl impl@(Implementation { implMethods }) = do
      implMethods' <- mapM n_implItem implMethods
      return impl { implMethods = implMethods' }

n_intfItem :: InterfaceItem -> Reassoc ()
n_intfItem (IntfOperator { intfOpName, intfOpPrec, intfOpAssoc }) = do
  intfOpPrec' <- prec intfOpPrec
  addOpInfo intfOpName (intfOpAssoc, intfOpPrec')

n_intfItem _ =
  return ()

n_implItem :: ImplementationItem -> Reassoc ImplementationItem
n_implItem = wrapFn n_implItem
  where
    n_implItem (ImplVar (a, e)) = do
      e' <- n_expr e
      return $ ImplVar (a, e')

    n_implItem fn@(ImplFunction { implBody }) = do
      implBody' <- n_codeBlock implBody
      return $ fn { implBody = implBody' }

    n_implItem op@(ImplOperator { implOpBody }) = do
      implOpBody' <- n_codeBlock implOpBody
      return $ op { implOpBody = implOpBody' }

prec :: Precedence -> Reassoc PrecInt
prec (PrecValue n) = return n
prec (PrecEqual n) =  getPrec n
prec (PrecHigher n) =  (+) 1 <$> getPrec n
prec (PrecLower n) =  (-) 1 <$> getPrec n

n_fn :: Function -> Reassoc Function
n_fn = wrapFn n_fn
  where
    n_fn fn = do
      body' <- n_codeBlock (body fn)
      return fn { body = body' }

n_expr :: Expr -> Reassoc Expr
n_expr = wrapFn n_expr'
  where
    n_expr' :: ASTNode BaseExpr String () -> Reassoc (ASTNode BaseExpr String ())
    n_expr' (BinOp _ _ ll lop (() :< BinOp _ _ rl rop rr)) = do
      ll' <- n_expr ll
      rl' <- n_expr rl
      rr' <- n_expr rr
      c <- comparePrec lop rop
      return $ case c of
        PLeft ->
          BinOp [] [] (() :< BinOp [] [] ll' lop rl') rop rr'
        PRight ->
          BinOp [] [] ll' lop (() :< BinOp [] [] rl' rop rr')

    n_expr' (Literal l) = return $ Literal l
    n_expr' (Ident a) = return $ Ident a

    n_expr' (ParenthesizedExpr e) =
      ParenthesizedExpr <$> n_expr e

    n_expr' (Match e cs) = do
      e' <- n_expr e
      cs' <- mapM n_case cs
      return $ Match e' cs'

    n_expr' (If cond conseq alt) = do
      cond' <- n_expr cond
      conseq' <- n_codeBlock conseq
      alt' <- n_codeBlock alt
      return $ If cond' conseq' alt'

    n_expr' call@(Call { callee, args }) = do
      callee' <- n_expr callee
      args' <- mapM n_expr args
      return $ call { callee = callee', args = args' }

    n_expr' op@(BinOp { lhs, rhs }) = do
      lhs' <- n_expr lhs
      rhs' <- n_expr rhs
      return $ op { lhs = lhs', rhs = rhs' }

    n_expr' (Record fields) = do
      fields' <- mapM (mapM n_expr) fields
      return $ Record fields'

    n_expr' (List t items) = do
      items' <- mapM n_expr items
      return $ List t items'

    n_expr' (FieldAccess obj field) = do
      obj' <- n_expr obj
      return $ FieldAccess obj' field

    n_expr' (FnExpr fn) =
      FnExpr <$> n_fn fn

    n_expr' (Negate cArgs e) = do
      e' <- n_expr e
      return $ Negate cArgs e'

    n_expr' VoidExpr = return VoidExpr

    n_expr' (TypeCall e cArgs) = do
      e' <- n_expr e
      return $ TypeCall e' cArgs

n_case :: Case -> Reassoc Case
n_case = wrapFn n_case
  where
    n_case (Case pattern body) = do
      body' <- n_codeBlock body
      return $ Case pattern body'

data Prec
  = PLeft
  | PRight

comparePrec :: String -> String -> Reassoc Prec
comparePrec l r = do
  (lAssoc, lPrec) <- getOpInfo l
  (rAssoc, rPrec) <- getOpInfo r
  case (compare lPrec rPrec, lAssoc, rAssoc) of
    (LT, _, _) -> return PRight
    (GT, _, _) -> return PLeft
    (EQ, AssocLeft, AssocLeft) -> return PLeft
    (EQ, AssocRight, AssocRight) -> return PRight
    (EQ, _, _) -> throwError $ PrecedenceError l r
