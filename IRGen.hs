module IRGen (generateIR) where

import AST

import qualified Data.Map as Map

import Control.Monad.RWS
import Data.Foldable (foldlM)
import Data.Maybe (isJust, fromJust)

import Debug.Trace

data Cmd =
    Binop Val Op Val Val
  | Unop Val Op Val
  | Call Val Val Int
  | Param Val
  | JmpIfTrue Val String
  | JmpIfFalse Val String
  | Jmp String
  | Ret Val
  | Copy Val Val
  | Label String
  deriving (Show)

data Op =
    Plus
  | Minus
  | Cmp
  | Lte | OLt | OGt
  | Gte
  | And
  | Or
  deriving (Show)

data Val =
    Const Integer
  | Reg Reg
  | Ref String
  deriving (Show)

type IR a = (RWS Env [Cmd] IRS) a

newtype Env = Env (Map.Map String Val)

newtype IRS = IRS { reg :: Int }

type Reg = Int

initEnv :: Env
initEnv = Env Map.empty

initIRS :: IRS
initIRS = IRS 0

generateIR :: AST -> [Cmd]
generateIR ast =
  snd $ evalRWS (g_program ast) initEnv initIRS

g_program :: AST -> IR ()
g_program (AProgram decls) = do
  g_decls decls
  return ()

g_decls :: [Decl] -> IR Val
g_decls [] = return (Const 0)
g_decls (DBind b:xs) = runBind b xs g_decls

g_binds :: [Bind] -> IR Val
g_binds [] = return (Const 0)
g_binds (b:xs) = runBind b xs g_binds

g_stmt :: Stmt -> IR (Maybe Val)
g_stmt (SExpr expr) =
  g_expr expr

g_expr :: Expr -> IR (Maybe Val)
g_expr (EFn fn) = do
  g_fn fn
  return Nothing

g_expr (ECall (EVar name) args) = do
  tmpReg <- genReg
  callee <- regOrRef name
  emit (Call tmpReg callee (length args))
  return $ Just tmpReg

g_fn (Fn params tyRet body) = do
  g_binds body
  emit $ Ret (Const 0)
  return Nothing

genReg :: IR Val
genReg = do
  s <- get
  put s{reg = reg s + 1}
  return $ Reg $ reg s

emit :: Cmd -> IR ()
emit cmd =
  tell [cmd]

regOrRef :: String -> IR Val
regOrRef label = do
  (Env env) <- ask
  return $ case Map.lookup label env of
             Just v -> v
             Nothing -> Ref label

extendEnv :: String -> Val -> Env -> Env
extendEnv name val (Env env) =
  Env $ Map.insert name val env

runBind :: Bind -> [a] -> ([a] -> IR Val) -> IR Val
runBind bind xs g =
  case bind of
    BStmt stmt ->
      do { g_stmt stmt
         ; g xs}

    BLet name stmt ->
      do { emit (Label name)
         ; r <- g_stmt stmt
         ; case r of
             Just reg ->
               local (extendEnv name reg) (g xs)
             Nothing -> g xs
         }
