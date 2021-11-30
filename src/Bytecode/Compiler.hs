module Bytecode.Compiler
  ( compile
  ) where

import Bytecode.Opcodes

import Absyn.Base (Literal(..))
import Core.Absyn
import Typing.Types (Type)

import Control.Monad.State (State, execState, gets, modify)
import Data.List (sortOn)
import qualified Data.Map as Map

data CompilerState
  = CS { currentBlock :: Block
       , blocks :: [Block]
       , blockId :: Int
       , lambdaId :: Int
       , typeId :: Int
       , typeMap :: Map.Map Type Int
       -- Local
       , varMap :: Map.Map String Register
       }

type BC a = State CompilerState a

initialState :: CompilerState
initialState = CS { currentBlock = Block "main" 0 1 0 [] []
                  , blocks = []
                  , blockId = 1
                  , lambdaId = 0
                  , typeId = 0
                  , typeMap = Map.empty
                  , varMap = Map.fromList [ ("ContVar \"halt\"", BlockRef $ -1)
                                          , ("Var \"#fix\"", BlockRef $ -2)]
                  }

compile :: Term -> Bytecode
compile term =
  let s = execState (compileTerm term) initialState
   in Bytecode $ sortOn Bytecode.Opcodes.id $ (currentBlock s) : (blocks s)

compileTerm :: Term -> BC ()
compileTerm (LetVal x v t) = do
  addLocal (show x) $ \x' -> do
    loadValue v x'
    compileTerm t

compileTerm (LetCont defs t) = do
  compileContDefs defs $ compileTerm t

compileTerm (LetFun defs t) = do
  compileFunDefs defs $ compileTerm t

compileTerm (AppCont k args) = do
  pushArgs args
  kAddress <- lookupVar (show k)
  numArgs <- addIntConstant $ length args
  emitOpcode (OpCall kAddress numArgs)

compileTerm (App f k args) = do
  pushArgs args
  kAddress <- lookupVar (show k)
  emitOpcode (OpPushReg kAddress)
  fAddress <- lookupVar (show f)
  numArgs <- addIntConstant (length args + 1)
  emitOpcode (OpCall fAddress numArgs)

compileTerm (Case x cases) = do
  x' <- lookupVar (show x)
  emitOpcode (OpJumpCase x')
  mapM_ emitCaseClause cases

compileTerm Error =
  emitOpcode OpError

addLocal :: String -> (Register -> BC a) -> BC a
addLocal x cont = do
  x' <- newLocal
  modify $ \s -> s { varMap = Map.insert x x' (varMap s) }
  cont x'

loadValue :: Value -> Register -> BC ()
loadValue Core.Absyn.Unit dst = do
  const <- addConstant Bytecode.Opcodes.Unit
  emitOpcode (OpLoadConst const dst)

loadValue (Lit l) dst = do
  const <- addConstant (Literal l)
  emitOpcode (OpLoadConst const dst)

loadValue (In key args) dst = do
  pushArgs args
  key' <- addConstant (Literal (String key))
  numArgs <- addIntConstant $ length args
  emitOpcode (OpMakeTaggedValue dst key' numArgs)

loadValue (Lam (Lambda k params body)) dst = do
  f <- makeLambdaName
  compileDefs [(f, show k : map show params, body)] $ do
    f' <- lookupVar f
    emitOpcode (OpMakeClosure dst f')

loadValue (Record fields) dst = do
  mapM_ pushField fields
  numFields <- addIntConstant $ length fields
  emitOpcode (OpMakeRecord dst numFields)
    where
      pushField (key, var) = do
        key' <- addConstant (Literal (String key))
        emitOpcode (OpPushConst key')
        reg <- lookupVar (show var)
        emitOpcode (OpPushReg reg)

loadValue (Type ty) dst = do
  typeTag <- makeTypeTag ty
  src <- addIntConstant typeTag
  emitOpcode (OpMakeType dst src)

makeTypeTag :: Type -> BC Int
makeTypeTag ty = do
  map <- gets typeMap
  case Map.lookup ty map of
    Just id -> return id
    Nothing -> do
      id <- gets typeId
      modify $ \s -> s { typeId = id + 1
                       , typeMap = Map.insert ty id map
                       }
      return id

makeLambdaName :: BC String
makeLambdaName = do
  id <- gets lambdaId
  modify $ \s -> s { lambdaId = id + 1 }
  return $ "lambda#" ++ show id

addConstant :: Constant -> BC Register
addConstant v = do
  consts <- constants <$> gets currentBlock
  updateCurrentBlock $ \block -> block { constants = consts ++ [v] }
  return $ Constant $ length consts

compileContDefs :: [ContDef] -> BC a -> BC ()
compileContDefs defs =
  compileDefs $ map unwrap defs
    where
      unwrap (ContDef k params body) = (show k, map show params, body)

compileFunDefs :: [FunDef] -> BC a -> BC ()
compileFunDefs defs =
  compileDefs $ map unwrap defs
    where
      unwrap (FunDef f k params body) = (show f, show k : map show params, body)

compileDefs :: [(String, [String], Term)] -> BC a -> BC ()
compileDefs [] scoped = do
  scoped
  return ()

compileDefs (d:ds) scoped = do
  compileDef d $ compileDefs ds scoped

compileDef :: (String, [String], Term) -> BC a -> BC a
compileDef (name, params, body) cont = do
  newBlock name $
    addParameters params $
      compileTerm body
  cont

newBlock :: String -> BC a -> BC ()
newBlock name ctor = do
  id <- gets blockId
  Constant k <- addIntConstant id
  oldCurrentBlock <- gets $ currentBlock
  oldVarMap <- gets $ varMap
  modify $ \cs -> cs { varMap = Map.insert name (BlockRef k) (varMap cs)
                     , blockId = id + 1
                     , currentBlock = Block name id 0 0 [] []
                     }
  ctor
  modify $ \cs -> cs { blocks = currentBlock cs : blocks cs
                     , currentBlock = oldCurrentBlock
                     , varMap = Map.insert name (BlockRef k) oldVarMap
                     }

addParameters :: [String] -> BC a -> BC ()
addParameters [] cont = do
  cont
  return ()

addParameters (p : ps) cont = do
  p' <- newParameter
  modify $ \s -> s { varMap = Map.insert p p' (varMap s) }
  addParameters ps cont

lookupVar :: String -> BC Register
lookupVar v = do
  m <- gets varMap
  return . fromJust m . Map.lookup v $ m
    where
      fromJust _ (Just a) = a
      fromJust m Nothing =
        error $ "lookupVar failed, couldn't find `" ++ v ++"` in `" ++ show m ++ "`"

emitOpcode :: Opcode -> BC ()
emitOpcode op = do
  updateCurrentBlock $ \block -> block { code = (code block) ++ [op] }

pushArgs :: [Var] -> BC ()
pushArgs = mapM_ pushArg . reverse
  where
    pushArg arg = do
      var <- lookupVar (show arg)
      emitOpcode (OpPushReg var)

newLocal :: BC Register
newLocal = do
  id <- locals <$> gets currentBlock
  updateCurrentBlock $ \b -> b { locals = id + 1 }
  return $ Local id

newParameter :: BC Register
newParameter = do
  id <- parameters <$> gets currentBlock
  updateCurrentBlock $ \b -> b { parameters = id + 1 }
  return $ Parameter id

updateCurrentBlock :: (Block -> Block) -> BC ()
updateCurrentBlock f =
  modify $ \s -> s { currentBlock = f (currentBlock s) }

emitCaseClause :: Clause -> BC ()
emitCaseClause _clause = do
  return ()

addIntConstant :: Int -> BC Register
addIntConstant i =
  addConstant (Literal (Integer $ toInteger i))
