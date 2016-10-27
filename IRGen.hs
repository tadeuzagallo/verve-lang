module IRGen (generateIR) where

import AST

import Control.Monad.State

data IR = IR [Cmd]
  deriving (Show)

data Cmd = 
  CBin Op Val Val
  | CUn Op Val
  | CCall Val Integer
  | CParam Reg
  | CLabel String
  | CJmpIfTrue Val String
  | CJmpIfFalse Val String
  | CJmp String
  | CRet
  deriving (Show)

data Op =
  OPlus
  | OMinus
  | OCmp
  | OLte | OLt | OGt
  | OGte
  | OAnd
  | OOr
  deriving (Show)

data Val =
  VNum Integer
  | VReg Reg
  | VLabel String
  deriving (Show)

data Reg = Reg Int
  deriving (Show)

data IRData = IRData Int [Cmd]
type IRS = State IRData

generateIR :: AST -> IR
generateIR ast =
  evalState (g_program ast >> gets (\(IRData _ ir) -> IR ir)) (IRData 0 [])

g_program (AProgram decls) =
  mapM g_decl decls

g_decl (DBind bind) =
  g_bind bind

g_bind (BLet label stmt) = do
  emit (CLabel label)
  g_stmt stmt

g_bind (BStmt stmt) =
  g_stmt stmt

g_stmt (SExpr expr) =
  g_expr expr

g_expr (EFn fn) =
  g_fn fn

g_expr (ECall (EVar name) args) = do
  emit (CCall (VLabel name) 0)

g_fn (Fn params tyRet body) = do
  mapM g_bind body
  emit CRet

genReg :: IRS Int
genReg  = do
  id <- gets $ \(IRData id _) -> id
  modify $ \(IRData id cmds) -> IRData (id + 1) cmds
  return id

emit :: Cmd -> IRS ()
emit cmd = 
  modify $ \(IRData id cmds) -> IRData id (cmd:cmds)
