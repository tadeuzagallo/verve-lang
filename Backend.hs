module Backend (x64) where

import IRGen

import qualified Data.Map as Map

import Control.Monad.State
import Data.List (sortOn)

import Debug.Trace

data X64Reg
  = Rdi
  | Rsi
  | Rdx
  | Rcx
  | R8
  | R9
  | UnusedReg
  deriving (Show)

type AddrMap = Map.Map Int Addr

data Addr
  = X64Reg X64Reg
  | StackOff Int
  deriving (Show)

data LiveInterval
  = LI { start :: Int, end :: Int }
  deriving (Show)

type LIM -- LiveIntervalMap
  = Map.Map Int LiveInterval

data LAS -- LiveAnalisysState
  = LAS { lasOff :: Int 
        , lasMap :: LIM
        }

type LiveAnalysis = State LAS

data RAS -- RegisterAllocationState
  = RAS { active :: [(X64Reg, LiveInterval)]
        , free :: [X64Reg]
        , registers :: AddrMap
        , stackOff :: Int
        }

type RA = State RAS

x64 :: [Cmd] -> String
x64 cmds = show (linearScanRegisterAllocation $ liveStates cmds)

linearScanRegisterAllocation :: LIM -> AddrMap
linearScanRegisterAllocation lim =
  registers $ snd $ runState (mapM_ allocateRegister (Map.toList lim)) initRAS

initRAS :: RAS
initRAS =
  RAS { active=[]
      , free=[Rdi, Rsi, Rdx, Rcx, R8, R9]
      , registers=Map.empty
      , stackOff=0
      }

allocateRegister :: (Reg, LiveInterval) ->  RA ()
allocateRegister (r, li) = do
  expireOldIntervals li
  f <- gets free
  if null f
     then spillAtInterval r
     else consume r li

expireOldIntervals :: LiveInterval -> RA ()
expireOldIntervals li = do
  ac <- gets active
  let (exp, ac') = filterExpired li [] ac
  modify $ \st ->
    st { active=ac'
       , free=exp++free st
       }

filterExpired :: LiveInterval -> [X64Reg] -> [(X64Reg, LiveInterval)] -> ([X64Reg], [(X64Reg, LiveInterval)])
filterExpired li exp ac =
  case ac of
    [] -> (exp, [])
    (reg, LI {end=e}):xs
      | e <= (end li) -> filterExpired li (reg:exp) xs
      | otherwise -> (exp, ac)

spillAtInterval :: Reg -> RA ()
spillAtInterval r = do
  modify $ \st ->
    st { registers=Map.insert r (StackOff $ stackOff st) (registers st)
       , stackOff=stackOff st + 1
       }

consume :: Reg -> LiveInterval -> RA ()
consume r li = do
  reg <- gets $ head . free
  modify $ \st ->
    st { free=tail (free st)
       , active= sortOn (end.snd) ((reg, li):active st)
       , registers=Map.insert r (X64Reg reg) (registers st)
       }


liveStates :: [Cmd] -> LIM
liveStates cmds = do
  let ls = \c -> count >> liveState c
      (_, st) = runState (mapM_ ls cmds) initLAS
   in lasMap st

initLAS :: LAS
initLAS =
  LAS { lasOff = 0
      , lasMap = Map.empty
      }

count :: LiveAnalysis ()
count =
  modify $ \st -> st{ lasOff = lasOff st + 1 }

liveState :: Cmd -> LiveAnalysis ()
liveState (Label _) = return ()

liveState (Arg r) = useR r

liveState (Call out v _) = do
  useR v
  initR out

liveState (Ret (Just v)) = useR v
liveState (Ret Nothing) = return ()

useR :: Val -> LiveAnalysis ()
useR (Reg r) =
  modify $ \st -> 
    st{ lasMap=Map.adjust (\li -> li{ end=lasOff st }) r (lasMap st) }

useR _ = return ()

initR :: Val -> LiveAnalysis ()
initR (Reg r) =
  modify $ \st -> 
    st{ lasMap=Map.insert r LI { start=(lasOff st), end=(lasOff st) }  (lasMap st) }

initR _ = return ()
