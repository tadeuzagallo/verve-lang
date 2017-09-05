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

import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    "--print-statements":file:_ -> runAndPrintStmts file
    file:_ -> runFile file

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

runFile :: String -> IO ()
runFile file = do
  result <- parseFile file
  -- TODO: add this as `Error::runError`
  either report putStrLn (run result)
  where
    run :: (Result Module) -> Result String
    run result = do
      absyn <- result
      balanced <- Naming.balance absyn
      (typed, ty) <- infer balanced
      let core = desugar typed
      val <- trace (show core) eval core
      -- TODO: move this printing into it's own function
      return $ ppr (val, ty)

runAndPrintStmts :: String -> IO ()
runAndPrintStmts file = do
  -- TODO: add this as `Error::runError`
  let mod = modNameFromFile file
  result <- execFile mod file
  either report (mapM_ putStrLn . reverse . snd) result

execFile :: String -> String -> IO (Result (EvalCtx, [String]))
execFile modName file = do
  result <- parseFile file
  either (return . Left) (runModule initEvalCtx file modName) result

runModule :: EvalCtx -> String -> String -> Module -> IO (Result (EvalCtx, [String]))
runModule ctx file modName (Module imports stmts) = do
  ctx' <- resolveImports ctx file imports
  either (return . Left) (\c -> foldM aux (c, []) stmts >>= return . Right) ctx'
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

resolveImports ctx file (imp@(Import _ mod _ _) : imports) = do
  let path = takeDirectory file </> joinPath mod <.> "vrv"
  res <- execFile (intercalate "." mod) path
  either (return . Left) (\(c, _) -> resolveImports (importModule imp ctx c) file imports) res

importModule :: Import -> EvalCtx -> EvalCtx -> EvalCtx
importModule imp (prevNenv, prevRnEnv, prevCtx, prevEnv) (impNenv, _, impCtx, impEnv) =
  ( Naming.nImportModule imp prevNenv impNenv
  , renameImport prevRnEnv imp
  , tImportModule imp prevCtx impCtx
  , iImportModule imp prevEnv impEnv
  )

modNameFromFile :: FilePath -> FilePath
modNameFromFile file =
  case dropExtension $ takeFileName file of
    [] -> undefined
    x:xs -> toUpper x : xs
