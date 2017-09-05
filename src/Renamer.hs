module Renamer
  ( RnEnv
  , initRnEnv
  , renameImport
  , renameStmt
  ) where

import Absyn.Untyped
import Error

import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Except (Except, runExcept)
import Data.List (intercalate)

import qualified Control.Monad.Except as Except (throwError)

data RenamerError
  = UnknownVariable String

instance Show RenamerError where
  show (UnknownVariable x) =
    "Unknown variable: " ++ x

instance ErrorT RenamerError where
  kind _ = "RenamerError"

throwError :: ErrorT a => a -> Rn b
throwError = Except.throwError . Error

type RdrName = (Maybe String, String)

data RnName
  = External (String, String)
  | Internal String

-- TODO: This will need scoping, otherwise the following will fail
--
-- module A { enum C { D } }
-- module B { enum D { C } }
--
-- then in a module C, when you import both A and B, the following will
-- be renamed from
--
-- f() -> (C, D) { (D, C) }
--
-- to:
--
-- f() -> (B.C, B.D) { (B.D, B.C) }
--
-- instead of:
--
-- f() -> (A.C, B.D) { (A.D, B.C) }

data RnEnv = RnEnv { getBinds :: [(RdrName, RnName)] }

initRnEnv :: RnEnv
initRnEnv = RnEnv { getBinds = mkBuiltins builtins }

mkBuiltins :: [String] -> [(RdrName, RnName)]
mkBuiltins =
  map $ \n ->
    ((Nothing, n), Internal n)

builtins :: [String]
builtins =
  -- TYPES
  [ "Int"
  , "Float"
  , "Char"
  , "String"
  , "Void"
  , "Bool"

  -- TYPE CONSTRUCTORS
  , "List"

  -- DATA CONSTRUCTORS
  , "True"
  , "False"
  , "Nil"
  , "Cons"

  -- FUNCTIONS
  , "int_add"
  , "int_sub"
  , "int_mul"
  , "int_div"
  , "int_print"
  ] 

data RnState = RnState { modName :: String }

type Rn a = (StateT RnState (Except Error) a)

initRn :: String -> RnState
initRn modName = RnState { modName = modName }

runRn :: String -> Rn a -> Result a
runRn mod m =
  runExcept $ evalStateT m (initRn mod)


-- ENV HELPERS

addLocal :: RnEnv -> String -> Rn (RnEnv, String)
addLocal env name = do
  mod <- thisMod
  let getBinds' = ((Nothing, name), External (mod, name)) : getBinds env
  let env' = RnEnv { getBinds = getBinds' }
  return (env', joinName (mod, name))

addInternal :: RnEnv -> String -> Rn RnEnv
addInternal env name = do
  let getBinds' = ((Nothing, name), Internal name) : getBinds env
  let env' = RnEnv { getBinds = getBinds' }
  return env'


joinName :: (String, String) -> String
joinName (mod, n) = mod ++ "." ++ n


lookupIdent :: [String] -> RnEnv -> Rn String
lookupIdent [] _ = undefined
lookupIdent [x] env = lookupRdrName (Nothing, x) env
lookupIdent xs env = 
  let mod = intercalate "." $ init xs
      name = last xs
   in lookupRdrName (Just mod, name) env


lookupRdrName :: RdrName -> RnEnv -> Rn String
lookupRdrName rdrName env =
  case lookup rdrName (getBinds env) of
    Nothing -> throwError $ UnknownVariable $ showRdrName rdrName
    Just (Internal n) -> return n
    Just (External n) -> return (joinName n)


showRdrName :: RdrName -> String
showRdrName (Nothing, name) = name
showRdrName (Just mod, name) = joinName (mod, name)


thisMod :: Rn String
thisMod =
  gets modName


-- ENTRY POINT

renameStmt :: String -> RnEnv -> Stmt -> Result (RnEnv, Stmt)
renameStmt modName env stmt =
  runRn modName $ r_stmt env stmt

renameImport :: RnEnv -> Import -> RnEnv
renameImport env _imp = env


-- ABSYN RENAMERS

r_stmts :: RnEnv -> [Stmt] -> Rn [Stmt]
r_stmts env stmts = snd <$> foldAcc r_stmt env stmts


r_stmt :: RnEnv -> Stmt -> Rn (RnEnv, Stmt)
r_stmt env (Expr expr) = do
  expr' <- r_expr env expr
  return (env, Expr expr')

r_stmt env (Enum name gen ctors) = do
  (envWithType, name') <- addLocal env name
  envWithGenerics <- foldM addInternal envWithType gen
  (envWithCtors, ctors') <- r_ctors envWithGenerics ctors
  return (envWithCtors, Enum name' gen ctors')

r_stmt env (FnStmt fn) = do
  (env', fn') <- r_fn env fn
  return (env', FnStmt fn')

r_stmt env (Let name expr) = do
  envWithVar <- addInternal env name
  expr' <- r_expr envWithVar expr
  return (envWithVar, Let name expr')

r_stmt env (Class name vars methods) = do
  (envWithClass, name') <- addLocal env name
  (_, vars') <- r_fnParams envWithClass vars
  envWithSelf <- addInternal env "self" 
  ((_, envWithoutSelf), methods') <- foldAcc r_method (envWithSelf, envWithClass) methods
  return (envWithoutSelf, Class name' vars' methods')

r_stmt env (Operator assoc prec gen lhs op rhs retType body) = do
  prec' <- r_prec env prec
  (envWithOp, op') <- addLocal env op
  (envWithGen, gen') <- r_generics envWithOp gen
  retType' <- r_type envWithGen retType

  (envWithParams, [lhs', rhs']) <- r_fnParams envWithGen [lhs, rhs]
  body' <- r_stmts envWithParams body

  let op = Operator assoc prec' gen' lhs' op' rhs' retType' body'
   in return (envWithOp, op)

r_stmt env (Interface name param methods) = do
  (envWithIntf, name') <- addLocal env name
  envWithParam <- addInternal env param
  ((_, envWithoutParam), methods') <- foldAcc r_fnDecl (envWithParam, envWithIntf) methods
  return (envWithoutParam, Interface name' param methods')

r_stmt env (Implementation name gen ty implMethods) = do
  name' <- lookupIdent [name] env
  (envWithGen, gen') <- r_generics env gen
  ty' <- r_type envWithGen ty
  (_, implMethods') <- foldAcc r_fnNonRec envWithGen implMethods
  return (env, Implementation name' gen' ty' implMethods')


r_expr :: RnEnv -> Expr -> Rn Expr
r_expr _ VoidExpr =
  return VoidExpr

r_expr _ (Literal l) =
  return (Literal l)

r_expr env (ParenthesizedExpr expr) =
  r_expr env expr

r_expr env (FnExpr fn) = do
  FnExpr . snd <$> r_fn env fn

r_expr env (Ident name ty) = do
  name' <- lookupIdent name env
  ty' <- r_type env ty
  return (Ident [name'] ty')

r_expr env (Match expr cases) = do
  expr' <- r_expr env expr
  cases' <- mapM (r_case env) cases
  return (Match expr' cases')

r_expr env (If cond conseq alt) = do
  cond' <- r_expr env cond
  conseq' <- r_stmts env conseq
  alt' <- r_stmts env alt
  return (If cond' conseq' alt')

r_expr env (Call callee _ tyArgs vArgs) = do
  callee' <- r_expr env callee
  tyArgs' <- mapM (r_type env) tyArgs
  vArgs' <- mapM (r_expr env) vArgs
  return (Call callee' [] tyArgs' vArgs')

r_expr env (BinOp _ _ lhs op rhs) = do
  lhs' <- r_expr env lhs
  op' <- lookupIdent [op] env
  rhs' <- r_expr env rhs
  return (BinOp [] [] lhs' op' rhs')

r_expr env (Record fields) = do
  fields' <- mapM (sequence . fmap (r_expr env)) fields
  return (Record fields')

r_expr env (List items) = do
  items' <- mapM (r_expr env) items
  return (List items')

r_expr env (FieldAccess expr ty field) = do
  expr' <- r_expr env expr
  ty' <- r_type env ty
  return (FieldAccess expr' ty' field)


r_case :: RnEnv -> Case -> Rn Case
r_case env (Case pat body) = do
  (env', pat') <- r_pat env pat
  body' <- r_stmts env' body
  return (Case pat' body')


r_pat :: RnEnv -> Pattern -> Rn (RnEnv, Pattern)
r_pat env PatDefault =
  return (env, PatDefault)

r_pat env (PatLiteral l) =
  return (env, PatLiteral l)

r_pat env (PatVar v) = do
  env' <- addInternal env v
  return (env', PatVar v)

r_pat env (PatCtor name args) = do
  name' <- lookupIdent [name] env
  (env', args') <- foldAcc r_pat env args
  return (env', PatCtor name' args')

r_type :: RnEnv -> Type -> Rn Type
r_type _ TVoid =
  return TVoid

r_type _ TPlaceholder =
  return TPlaceholder

r_type env (TName name) = do
  name' <- lookupIdent [name] env
  return $ TName name'

r_type env (TApp ty args) = do
  ty' <- r_type env ty
  args' <- mapM (r_type env) args
  return $ TApp ty' args'

r_type env (TArrow params ret) = do
  params' <- mapM (r_type env) params
  ret' <- r_type env ret
  return $ TArrow params' ret'

r_type env (TRecord fields) = do
  fields' <- mapM (sequence . fmap (r_type env)) fields
  return $ TRecord fields'

r_fnNonRec :: RnEnv -> Function -> Rn (RnEnv, Function)
r_fnNonRec = r_fnBase False

r_fn :: RnEnv -> Function -> Rn (RnEnv, Function)
r_fn = r_fnBase True

r_fnBase :: Bool -> RnEnv -> Function -> Rn (RnEnv, Function)
r_fnBase isRec env (Function name gen params retType body) = do
  (envWithFn, name') <- addLocal env name
  let env' = if isRec
      then envWithFn
      else env
  (envWithGenerics, gen') <- r_generics env' gen
  (envWithParams, params') <- r_fnParams envWithGenerics params
  retType' <- r_type envWithGenerics retType
  body' <- r_stmts envWithParams body

  let fn = Function name' gen' params' retType' body'
   in return (env', fn)


r_fnParams :: RnEnv -> [Param] -> Rn (RnEnv, [Param])
r_fnParams = foldAcc r_fnParam


r_fnParam :: RnEnv -> Param -> Rn (RnEnv, Param)
r_fnParam env (name, ty) = do
  envWithParam <- addInternal env name
  ty' <- r_type env ty
  return (envWithParam, (name, ty'))


r_generics :: RnEnv -> Generics -> Rn (RnEnv, Generics)
r_generics = foldAcc r_generic


r_generic :: RnEnv -> (Name, [Type]) -> Rn (RnEnv, (Name, [Type]))
r_generic env (name, bounds) = do
  envWithTypeVar <- addInternal env name
  bounds' <- mapM (r_type env) bounds
  return (envWithTypeVar, (name, bounds'))


r_ctors :: RnEnv -> [DataCtor] -> Rn (RnEnv, [DataCtor])
r_ctors = foldAcc r_ctor


r_ctor :: RnEnv -> DataCtor -> Rn (RnEnv, DataCtor)
r_ctor env (name, args) = do
  (env', name') <- addLocal env name
  args' <- sequence $ args >>= return . mapM (r_type env)
  return (env', (name', args'))


foldAcc :: Monad m => (a -> b -> m (a, b)) -> a -> [b] -> m (a, [b])
foldAcc f a bs =
  let aux (a, bs) b = do
        (a', b') <- f a b
        return (a', b' : bs)
   in fmap reverse <$> foldM aux (a, []) bs

r_prec :: RnEnv -> Precedence -> Rn Precedence
r_prec _ (PrecValue v) =
  return $ PrecValue v

r_prec env (PrecHigher name) = do
  name' <- lookupIdent [name] env
  return (PrecHigher name')

r_prec env (PrecLower name) = do
  name' <- lookupIdent [name] env
  return (PrecLower name')

r_prec env (PrecEqual name) = do
  name' <- lookupIdent [name] env
  return (PrecEqual name')


r_method :: (RnEnv, RnEnv) -> Function -> Rn ((RnEnv, RnEnv), Function)
r_method (envWithSelf, envWithoutSelf) fn = do
  (envWithSelf', fn') <- r_fn envWithSelf fn
  (envWithoutSelf', _) <- addLocal envWithoutSelf (name fn)
  return ((envWithSelf', envWithoutSelf'), fn')

r_fnDecl :: (RnEnv, RnEnv) -> FunctionDecl -> Rn ((RnEnv, RnEnv), FunctionDecl)
r_fnDecl (envWithParam, envWithoutParam) (FunctionDecl name gen params retType) = do
  (envWithParam', name') <- addLocal envWithParam name
  (envWithoutParam', _) <- addLocal envWithoutParam name
  (envWithGenerics, gen') <- r_generics envWithParam' gen
  (envWithParams, params') <- r_fnParams envWithGenerics params
  retType' <- r_type envWithParams retType
  let fnDecl = FunctionDecl name' gen' params' retType'
   in return ((envWithParam', envWithoutParam'), fnDecl)
