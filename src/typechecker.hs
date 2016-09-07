module TypeChecker (type_check) where

import AST
import Type

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
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

typeof Function { params=params, ret_type=(Just ret_type), body=body } = do
  typeof body
  function_type params ret_type

typeof FunctionType { parameters=params, return_type=ret_type } =
  function_type params ret_type

typeof (Extern prototype) = typeof prototype

typeof Prototype { prototype=fn_type } = typeof fn_type

typeof Call { callee=callee, arguments=args } = do
  (TyFunction params ret_type) <- typeof callee
  when (length params /= length args) (throwError "Wrong number of arguments for function call")
  args' <- mapM typeof args
  mapM_ (uncurry tyeqv) (zip args' params)
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
tyeqv t1 t2 =
  case (t1, t2) of
    (TyChar, TyChar) -> return ()
    (TyInt, TyInt) -> return ()
    (TyFloat, TyFloat) -> return ()
    (TyVoid, TyVoid) -> return ()
    (TyBool, TyBool) -> return ()
    (TyString, TyString) -> return ()
    (_, _) -> throwError "Invalid type for argument"

function_type :: [AST] -> AST -> TypeCheckerState
function_type params ret_type =
  TyFunction <$> (mapM typeof params) <*> (typeof ret_type)
