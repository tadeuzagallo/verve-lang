import Absyn (Module(Module), Name, Stmt, UnresolvedType)
import Error
import Parser
import qualified Naming
import Desugar
import Interpreter
import Typing.Ctx
import Typing.TypeChecker

import Control.Monad (foldM_)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
       (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    "--print-statements":file:_ -> runAndPrintStmts file
    file:_ -> runFile file

type EvalCtx = (Naming.Env, Ctx, Env)

initEvalCtx :: EvalCtx
initEvalCtx = (Naming.defaultEnv, defaultCtx, defaultEnv)

evalStmt ::  EvalCtx -> Stmt Name UnresolvedType -> Result (EvalCtx, String)
evalStmt (nenv, ctx, env) stmt = do
  (nenv', balanced) <- Naming.balanceStmt nenv stmt
  (ctx', typed, ty) <- inferStmt ctx balanced
  let core = desugarStmt typed
  (env', val) <- evalWithEnv env core
  let output = printf "%s : %s" (show val) (show ty)
  return ((nenv', ctx', env'), output)

repl :: IO ()
repl = runInputT defaultSettings $ loop initEvalCtx
  where
    loop :: EvalCtx -> InputT IO ()
    loop ctx = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "" -> outputStrLn "" >> loop ctx
        Just input ->
          case result ctx input of
            Left err -> do
              liftIO $ report err
              loop ctx
            Right (ctx', output) ->
              outputStrLn output >> loop ctx'
    result :: EvalCtx -> String -> Result (EvalCtx, String)
    result ctx input = do
      stmt <- parseStmt "(stdin)" input
      evalStmt ctx stmt

runFile :: String -> IO ()
runFile file = do
  result <- parseFile file
  -- TODO: add this as `Error::runError`
  either report putStrLn (run result)
  where
    run :: (Result (Module Name UnresolvedType)) -> Result String
    run result = do
      absyn <- result
      balanced <- Naming.balance absyn
      (typed, ty) <- infer balanced
      let core = desugar typed
      val <- eval core
      -- TODO: move this printing into it's own function
      return $ printf "%s : %s" (show val) (show ty)

runAndPrintStmts :: String -> IO ()
runAndPrintStmts file = do
  result <- parseFile file
  -- TODO: add this as `Error::runError`
  either report id (run result)
  where
    run :: (Result (Module Name UnresolvedType)) -> Result (IO ())
    run result = do
      (Module stmts) <- result
      return $ foldM_ aux initEvalCtx stmts
    aux :: EvalCtx -> Stmt Name UnresolvedType -> IO EvalCtx
    aux ctx stmt =
      case evalStmt ctx stmt of
        Left err -> do
          report err
          return ctx
        Right (ctx', out) -> do
          putStrLn out
          return ctx'
