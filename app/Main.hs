import Absyn (Module, UnresolvedType)
import Error
import Parser
import qualified Naming
import TypeChecker
import Desugar
import Interpreter

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
    file:_ -> runFile file

repl :: IO ()
repl = runInputT defaultSettings $ loop (Naming.defaultEnv, defaultCtx, defaultEnv)
  where
    loop :: (Naming.Env, Ctx, Env) -> InputT IO ()
    loop (nenv, ctx, env) = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "" -> outputStrLn "" >> loop (nenv, ctx, env)
        Just input ->
          case result nenv ctx env input of
            Left err -> do
              liftIO $ report err
              loop (nenv, ctx, env)
            Right (nenv', ctx', env', output) ->
              outputStrLn output >> loop (nenv', ctx', env')
    result :: Naming.Env -> Ctx -> Env -> String -> Either Error (Naming.Env, Ctx, Env, String)
    result nenv ctx env input = do
      stmt <- parseStmt "(stdin)" input
      (nenv', balanced) <- Naming.balanceStmt nenv stmt
      (ctx', typed, ty) <- inferStmt ctx balanced
      let core = desugarStmt typed
      (env', val) <- evalWithEnv env core
      let output = printf "%s : %s" (show val) (show ty)
      return (nenv', ctx', env', output)

runFile :: String -> IO ()
runFile file = do
  result <- parseFile file
  -- TODO: add this as `Error::runError`
  either report putStrLn (run result)
  where
    run :: (Either Error (Module String UnresolvedType)) -> Either Error String
    run result = do
      absyn <- result
      balanced <- Naming.balance absyn
      (typed, ty) <- infer balanced
      let core = desugar typed
      val <- eval core
      -- TODO: move this printing into it's own function
      return $ printf "%s : %s" (show val) (show ty)
