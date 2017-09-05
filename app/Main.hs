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


type EvalCtx = (Naming.Env, RnEnv, Ctx, Env)

data Config = Config
  { dumpCore :: Bool
  , printStatements :: Bool
  }

defaultConfig :: Config
defaultConfig = Config { dumpCore = False
                       , printStatements = False
                       }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl

    "--print-statements":file:_ ->
      let config = defaultConfig { printStatements = True }
       in runFile config file

    "--dump-core":file:_ ->
      let config = defaultConfig { dumpCore = True }
       in runFile config file

    file:_ ->
      runFile defaultConfig file


initEvalCtx :: EvalCtx
initEvalCtx = (Naming.defaultEnv, initRnEnv, defaultCtx, defaultEnv)


evalStmt :: Config ->  String -> EvalCtx -> Stmt -> Result (EvalCtx, String)
evalStmt config modName (nenv, rnEnv, ctx, env) stmt = do
  (nenv', balanced) <- Naming.balanceStmt nenv stmt
  (rnEnv', renamed) <- renameStmt modName rnEnv balanced
  (ctx', typed, ty) <- inferStmt ctx renamed
  let core = desugarStmt typed
  (env', val) <- evalWithEnv env core
  let out = if dumpCore config
              then show core
              else ppr (val, ty)
  return ((nenv', rnEnv', ctx', env'), out)


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
      evalStmt defaultConfig "REPL" ctx stmt


runFile :: Config -> String -> IO ()
runFile config file = do
  -- TODO: add this as `Error::runError`
  let mod = modNameFromFile file
  result <- execFile config mod file
  either (mapM_ report) printOutput result
    where
      printOutput (_ctx, []) = return ()
      printOutput (_ctx, out) =
        if printStatements config || dumpCore config
           then mapM_ putStrLn (reverse out)
           else putStrLn (head out)


execFile :: Config -> String -> String -> IO (Either [Error] (EvalCtx, [String]))
execFile config modName file = do
  result <- parseFile file
  either (return . Left) (runModule config file modName) result


runModule :: Config -> String -> String -> Module -> IO (Either [Error] (EvalCtx, [String]))
runModule config file modName (Module imports stmts) = do
  ctx <- resolveImports initEvalCtx file imports
  either (return . Left) execModule ctx
  where
    aux :: (EvalCtx, [Either Error String]) -> Stmt -> IO (EvalCtx, [Either Error String])
    aux (ctx, msgs) stmt =
      case evalStmt config modName ctx stmt of
        Left err -> do
          return (ctx, Left err : msgs)
        Right (ctx', out) -> do
          return (ctx', Right out : msgs)

    execModule :: EvalCtx -> IO (Either [Error] (EvalCtx, [String]))
    execModule ctx = do
      (ctx', output) <- foldM aux (ctx, []) stmts
      let (errMessages, outMessages) = foldl part ([], []) output
      if dumpCore config
         then return $ Right (ctx', reverse $ outMessages ++ map showError errMessages)
         else return $ case errMessages of
                         [] -> Right (ctx', reverse outMessages)
                         _ -> Left (reverse errMessages)

    part :: ([Error], [String]) -> Either Error String -> ([Error], [String])
    part (errs, succs) (Left err) =
      if printStatements config
         then (errs, showError err : succs)
         else (err : errs, succs)
    part (errs, succs) (Right suc) = (errs, suc : succs)


resolveImports :: EvalCtx -> String -> [Import] -> IO (Either [Error] EvalCtx)
resolveImports ctx _ [] =
  return . return $ ctx

resolveImports ctx file (imp@(Import _ mod _ _) : remainingImports) = do
  let path = takeDirectory file </> joinPath mod <.> "vrv"
  res <- execFile defaultConfig (intercalate "." mod) path
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
