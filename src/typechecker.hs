module TypeChecker (type_check) where

import AST
import Type

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import Data.Maybe (isNothing, fromJust)
import Text.Printf (printf)

type Context = (Map.Map String Type)
type Error = String
type TypeCheckerMonad = ExceptT Error (State Context)
type TypeCheckerState = TypeCheckerMonad Type

type_check :: AST -> Either String Type
type_check node = evalState (runExceptT $ typeof node) Map.empty

bind :: AST -> TypeCheckerState
bind ast = do
  ctx <- get
  t <- typeof ast
  case ast of
    Function { name=name } -> do
      put (Map.insert name t ctx)
      return t
    Extern Prototype { name=name } -> do
      put (Map.insert name t ctx)
      return t
    _ -> return t

typeof :: AST -> TypeCheckerState
typeof Program { expressions=body } =
  foldlM (flip $ const . bind) TyVoid body

typeof (Block nodes) =
  foldlM (flip $ const . bind) TyVoid nodes

typeof (Number (Left _)) = return TyInt
typeof (Number (Right _)) = return TyFloat
typeof (String _) = return TyString

typeof (Identifier name) = do
  value <- gets (Map.lookup name)
  maybe throw return value
    where throw = throwError $ printf "Unknown identifier: `%s`" name

typeof (BasicType t) = do
  value <- gets (Map.lookup t)
  case t of
    "char" -> return TyChar
    "int" -> return TyInt
    "float" -> return TyFloat
    "void" -> return TyVoid
    {- TODO: Declare types on prelude -}
    "bool" -> return TyBool
    "string" -> return TyString
    _ -> maybe throw return value
      where throw = throwError (printf "Unknown type: `%s`" t)

typeof Function { params=params, variables=variables, ret_type=(Just ret_type), body=body } = do
  ctx <- get
  load_type_variables variables
  ty_body <- typeof body
  ty_ret <- typeof ret_type
  tyeqv ty_body ty_ret
  t <- function_type params ret_type
  put ctx
  return t

typeof FunctionType { parameters=params, return_type=ret_type } =
  function_type params ret_type

typeof (Extern prototype) = typeof prototype

typeof Prototype { prototype=fn_type } = typeof fn_type

typeof Call { callee=callee, arguments=args } = do
  ctx <- get
  (TyFunction params ret_type) <- typeof callee
  when (length params /= length args) (throwError "Wrong number of arguments for function call")
  args' <- mapM typeof args
  mapM_ (uncurry tyeqv) (zip args' params)
  put ctx
  return ret_type

typeof FunctionParameter { type'=(Just t) } = typeof t

typeof BinaryOp { lhs=lhs, rhs=rhs } = do
  lhs' <- typeof lhs
  rhs' <- typeof rhs
  case (lhs', rhs') of
    (TyInt, TyInt) -> return TyInt
    (TyFloat, TyFloat) -> return TyFloat
    (_, _) -> throwError "Binary operations can only happen on two integers or two floats"

typeof t = throwError ("Unhandled node: " ++ (show t))

tyeqv :: Type -> Type -> (TypeCheckerMonad ())
tyeqv t1 t2 = do
  t1' <- simplify t1
  t2' <- simplify t2
  ctx <- get
  case (t1', t2') of
    (TyChar, TyChar) -> return ()
    (TyInt, TyInt) -> return ()
    (TyFloat, TyFloat) -> return ()
    (TyVoid, TyVoid) -> return ()
    (TyBool, TyBool) -> return ()
    (TyString, TyString) -> return ()
    (TyGeneric a, TyGeneric b) | a == b -> return ()
    (t, TyEmptyGeneric a) -> do
      put $ Map.insert a t ctx
      return ()
    (TyEmptyGeneric a, t) -> do
      put $ Map.insert a t ctx
      return ()
    (t, TyGeneric a) | fromJust (Map.lookup a ctx) /= t2 -> do
      let a' = fromJust (Map.lookup a ctx)
       in tyeqv t a'
    (TyGeneric a, t) | fromJust (Map.lookup a ctx) /= t1 -> do
      let a' = fromJust (Map.lookup a ctx)
       in tyeqv a' t
    (_, _) -> throwError $ printf "Invalid type: expected `%s` but received `%s`" (show t1) (show t2)

simplify :: Type -> TypeCheckerState
simplify generic@(TyGeneric name) = do
  t <- gets $ Map.lookup name
  case t of
    Nothing -> return generic
    Just t | t == generic -> return (TyEmptyGeneric name)
    Just t -> return t
simplify t = return t

function_type :: [AST] -> AST -> TypeCheckerState
function_type params ret_type =
  TyFunction <$> (mapM typeof params) <*> (typeof ret_type)

load_type_variables :: Maybe [String] -> (TypeCheckerMonad ())
load_type_variables Nothing = return ()
load_type_variables (Just vars) = do
  ctx <- get
  ctx' <- foldlM add_var ctx vars
  put ctx'
  return ()
  -- add_var :: Context -> String -> (MonadState ())
    where add_var = \ctx str -> do {
          str' <- uniquify str;
          return $ Map.insert str' (TyGeneric str') ctx
                                   }

uniquify :: String -> (TypeCheckerMonad String)
uniquify var = do
  t <- gets $ Map.lookup var
  case t of
    Nothing -> return var
    Just t -> uniquify (var ++ "'")
