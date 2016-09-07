module TypeChecker (type_check) where

import AST
import Type

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import Text.Printf (printf)

type Context = (Map.Map String Type)
type Error = String
type TypeCheckerState = StateT Context (ExceptT String Identity) Type

type_check :: AST -> Either String Type
type_check node = runIdentity $ runExceptT (evalStateT (typeof node) Map.empty)

bind :: AST -> TypeCheckerState
bind ast = do
  ctx <- get
  t <- typeof ast
  case ast of
    Function { name=name } ->
      put (Map.insert name t ctx) >> return t
    Extern (Prototype name _) ->
      put (Map.insert name t ctx) >> return t
    _ -> return t

typeof :: AST -> TypeCheckerState
typeof (Program _ body) =
  foldlM (\_ ast -> bind ast) TyVoid body

typeof (Block nodes) =
  foldlM (\_ ast -> bind ast) TyVoid nodes

typeof (Number (Left _)) = return TyInt
typeof (Number (Right _)) = return TyFloat
typeof (String _) = return TyString
typeof (Identifier name) = do
  value <- gets (Map.lookup name)
  case value of
    Nothing -> throwError (printf "Unknown identifier: `%s`" name)
    Just t -> return t

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
    _ -> (case value of
           Nothing -> throwError (printf "Unknown type: `%s`" t)
           Just t -> return t)

typeof (Function name generics params (Just ret_type) body) = do
  typeof body
  function_type params ret_type

typeof (FunctionType generics params ret_type) =
  function_type params ret_type

typeof (Extern prototype) = typeof prototype

typeof (Prototype name fn_type) = typeof fn_type

typeof (Call callee args) = do
  (TyFunction params ret_type) <- typeof callee
  if length params /= length args then
                                  throwError "Wrong number of arguments for function call"
                                  else
                                  do {
                                     args' <- mapM typeof args;
                                     mapM_ (uncurry tyeqv) (zip args' params);
                                     return ret_type
                                     }

typeof (FunctionParameter _ _ (Just t)) = typeof t

typeof BinaryOp { lhs=lhs, rhs=rhs } = do
  lhs' <- typeof lhs
  rhs' <- typeof rhs
  case (lhs', rhs') of
    (TyInt, TyInt) -> return TyInt
    (TyFloat, TyFloat) -> return TyFloat
    (_, _) -> throwError "Binary operations can only happen on two integers or two floats"

typeof t = throwError ("Unhandled node: " ++ (show t))

tyeqv :: Type -> Type -> StateT Context (ExceptT String Identity) ()
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
