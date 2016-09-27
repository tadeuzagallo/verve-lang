module Naming (naming) where

import AST
import Bind

import qualified Data.Map as Map

import Control.Monad (liftM)
import Control.Monad.State (State, evalState, gets, modify, get, put)

type Context = (Map.Map String (Bind String))
type NamingState = State Context

naming :: Program String -> Program String
naming program = evalState (naming_program program) Map.empty


naming_program :: Program String -> NamingState (Program String)
naming_program (Program imports decls) = do
  imports' <- mapM naming_import imports
  decls' <- mapM naming_decl decls
  return $ Program imports' decls'

naming_import :: Import String -> NamingState (Import String)
naming_import imp = return imp

naming_decl :: TopDecl String -> NamingState (TopDecl String)
naming_decl (InterfaceDecl interface) =
  InterfaceDecl <$> naming_interface interface 

naming_decl (ImplementationDecl implementation) =
  ImplementationDecl <$> naming_implementation implementation

naming_decl (ExternDecl prototype) =
  ExternDecl <$> naming_prototype prototype

naming_decl (TypeDecl enum_type) =
  return $ TypeDecl enum_type

naming_decl (ExprDecl expr) =
  ExprDecl <$> naming_expr expr

naming_interface :: Interface String -> NamingState (Interface String)
naming_interface interface = return interface

naming_implementation :: Implementation String -> NamingState (Implementation String)
naming_implementation implementation = return implementation

naming_prototype :: Prototype String -> NamingState (Prototype String)
naming_prototype prototype = return prototype

naming_expr :: Expr String -> NamingState (Expr String)
naming_expr (FunctionExpr fn) =
  FunctionExpr <$> naming_function fn

naming_expr var@(Var lname@(Loc _ name)) = do
  bind <- gets $ Map.lookup name
  case bind of
    Nothing -> return var
    Just (ParamBind (FunctionParameter _ index _)) ->
      return $ Arg lname index

naming_expr (LiteralExpr lit) =
  LiteralExpr <$> naming_literal lit

naming_expr expr = return expr

naming_function :: Function String -> NamingState (Function String)
naming_function fn@Function { params=params, body=body } = do
  let params' = foldl (\a (FunctionParameter n _ t) -> a ++ [FunctionParameter n (length a) t]) [] params
  ctx <- get
  mapM naming_fn_param params'
  body' <- naming_block body
  put ctx
  return fn { params=params', body=body' }

naming_fn_param :: FunctionParameter String -> NamingState (FunctionParameter String)
naming_fn_param param@(FunctionParameter (Loc _ name) _ _) = do
  modify $ Map.insert name (ParamBind param)
  return param

naming_block :: Block String -> NamingState (Block String)
naming_block (Block exprs) =
  Block <$> mapM naming_expr exprs

naming_literal :: Literal String -> NamingState (Literal String)
naming_literal lit = return lit
