import Absyn.Untyped
import Error
import Parser
import qualified Naming
import Desugar
import Renamer
import Interpreter
import Typing.Ctx
import Typing.TypeChecker
import PrettyPrint

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Console.Haskeline
       (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), (<.>), takeDirectory, joinPath, takeFileName, dropExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    "--print-statements":file:_ -> runFile True file
    file:_ -> runFile False file

type EvalCtx = (Naming.Env, RnEnv, Ctx, Env)

initEvalCtx :: EvalCtx
initEvalCtx = (Naming.defaultEnv, initRnEnv, defaultCtx, defaultEnv)

evalStmt ::  String -> EvalCtx -> Stmt -> Result (EvalCtx, String)
evalStmt modName (nenv, rnEnv, ctx, env) stmt = do
  (nenv', balanced) <- Naming.balanceStmt nenv stmt
  (rnEnv', renamed) <- renameStmt modName rnEnv balanced
  (ctx', typed, ty) <- inferStmt ctx renamed
  let core = desugarStmt typed
  (env', val) <- evalWithEnv env core
  return ((nenv', rnEnv', ctx', env'), ppr (val, ty))

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
      evalStmt "" ctx stmt

runFile :: Bool -> String -> IO ()
runFile shouldPrint file = do
  -- TODO: add this as `Error::runError`
  let mod = modNameFromFile file
  result <- execFile mod file
  either report printOutput result
    where
      printOutput (_ctx, []) = return ()
      printOutput (_ctx, out) =
        if shouldPrint
           then mapM_ putStrLn (reverse out)
           else putStrLn (head out)


execFile :: String -> String -> IO (Result (EvalCtx, [String]))
execFile modName file = do
  result <- parseFile file
  either (return . Left) (runModule file modName) result

runModule :: String -> String -> Module -> IO (Result (EvalCtx, [String]))
runModule file modName (Module imports stmts) = do
  ctx <- resolveImports initEvalCtx file imports
  either (return . Left) (\c -> foldM aux (c, []) stmts >>= return . Right) ctx
  where
    aux :: (EvalCtx, [String]) -> Stmt -> IO (EvalCtx, [String])
    aux (ctx, msgs) stmt =
      case evalStmt modName ctx stmt of
        Left err -> do
          return (ctx, showError err : msgs)
        Right (ctx', out) -> do
          return (ctx', out : msgs)

resolveImports :: EvalCtx -> String -> [Import] -> IO (Result EvalCtx)
resolveImports ctx _ [] =
  return . return $ ctx

resolveImports ctx file (imp@(Import _ mod _ _) : remainingImports) = do
  let path = takeDirectory file </> joinPath mod <.> "vrv"
  res <- execFile (intercalate "." mod) path
  either (return . Left) processCtx res
    where
      processCtx (newCtx, _output) =
        let ctx' = importModule imp ctx newCtx
         in resolveImports ctx' file remainingImports

importModule :: Import -> EvalCtx -> EvalCtx -> EvalCtx
importModule imp (prevNenv, prevRnEnv, prevCtx, prevEnv) (impNenv, impRnEnv, impCtx, impEnv) =
  let (rnEnv', renamedImports) = renameImport prevRnEnv impRnEnv imp
   in ( Naming.nImportModule imp prevNenv impNenv
      , rnEnv'
      , tImportModule renamedImports prevCtx impCtx
      , iImportModule imp prevEnv impEnv
      )

modNameFromFile :: FilePath -> FilePath
modNameFromFile file =
  case dropExtension $ takeFileName file of
    [] -> undefined
    x:xs -> toUpper x : xs
