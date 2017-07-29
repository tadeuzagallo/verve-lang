module Interpreter
  ( eval
  ) where

import Absyn

import System.IO.Unsafe (unsafePerformIO)

type EvalResult = Either RuntimeError Value

data Env = Env
  { globals :: [(String, Value)]
  , locals :: [Value]
  } deriving (Show)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  deriving (Show)

data Value
  = VLit Literal
  | VLam (Env -> EvalResult)
  | Void

instance Show Value where
  show (VLit v) = show v
  show (VLam _) = "<function>"
  show Void = ""

builtins :: [(String, Value)]
builtins =
  [ ( "int_add"
    , VLam
        (\(Env {locals = (VLit (Integer a)):(VLit (Integer b)):_}) ->
           return . VLit . Integer $ a + b))
  , ( "int_print"
    , VLam
        (\(Env {locals = v:_}) ->
           case unsafePerformIO (print v) of
             () -> return Void))
  ]

defaultEnv :: Env
defaultEnv = Env {globals = builtins, locals = []}

addLocals :: Env -> [Value] -> Env
addLocals env locals' = env {locals = locals' ++ (locals env)}

eval :: Expr -> EvalResult
eval = e_expr defaultEnv

e_expr :: Env -> Expr -> EvalResult
e_expr _ (Literal s) = return $ VLit s
e_expr env (Ident id) =
  case lookup id (globals env) of
    Nothing -> Left $ UnknownVariable id
    Just val -> return val
e_expr env (App fn args) = do
  args' <- mapM (e_expr env) args
  VLam f <- e_expr env fn
  let env' = addLocals env args'
  f env'
e_expr _ _ = Left Unsupported
