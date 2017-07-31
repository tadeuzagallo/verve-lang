import Interpreter
import Parser
import TypeChecker

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
        Just "" -> do
          outputStrLn ""
          loop (ctx, env)
        Just "quit" -> return ()
        Just input -> do
          let result = parseStmt "(stdin)" input
          case result of
            Left err -> outputStrLn $ show err
            Right stmt ->
              case inferStmt ctx stmt of
                Left err -> outputStrLn $ show err
                Right (ctx', ty) ->
                  case evalStmt env stmt of
                    Left err -> outputStrLn $ show err
                    Right (env', val) -> do
                      outputStrLn $ printf "%s : %s" (show val) (show ty)
                      loop (ctx', env')

runFile :: String -> IO ()
runFile file = do
  result <- parseFile file
  case result of
    Left err -> print err
    Right absyn ->
      case infer absyn of
        Left err -> print err
        Right ty ->
          case eval absyn of
            Left err -> print err
            Right value -> printf "%s : %s\n" (show value) (show ty)
