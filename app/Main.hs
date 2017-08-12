import Absyn (Module, UnresolvedType)
import Error
import Interpreter
import Parser
import TypeChecker
import Desugar

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
repl = runInputT defaultSettings $ loop (defaultCtx, defaultEnv)
  where
    loop :: (Ctx, Env) -> InputT IO ()
    loop (ctx, env) = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "" -> outputStrLn "" >> loop (ctx, env)
        Just input ->
          case result ctx env input of
            Left err -> do
              liftIO $ report err
              loop (ctx, env)
            Right (ctx', env', output) ->
              outputStrLn output >> loop (ctx', env')
    result :: Ctx -> Env -> String -> Either Error (Ctx, Env, String)
    result ctx env input = do
      stmt <- parseStmt "(stdin)" input
      (ctx', stmt', ty) <- inferStmt ctx stmt
      let expr = desugarStmt stmt'
      (env', val) <- evalWithEnv env expr
      let output = printf "%s : %s" (show val) (show ty)
      return (ctx', env', output)

runFile :: String -> IO ()
runFile file = do
  result <- parseFile file
  -- TODO: add this as `Error::runError`
  either report putStrLn (run result)
  where
    run :: (Either Error (Module String UnresolvedType)) -> Either Error String
    run result = do
      absyn <- result
      (absyn', ty) <- infer absyn
      let core = desugar absyn'
      val <- eval core
      -- TODO: move this printing into it's own function
      return $ printf "%s : %s" (show val) (show ty)
