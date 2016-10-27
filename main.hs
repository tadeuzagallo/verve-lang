import Parser
import IRGen

import Options.Applicative

data Cmd = Cmd { file :: String }

main :: IO ()
main =
  execParser opts >>= runCmd
    where
      opts = info (helper <*> parseArgs) (fullDesc <> header "Verve compiler")

runCmd :: Cmd -> IO ()
runCmd (Cmd file) = do
  pRes <- parse file
  case pRes of
    Left e -> putStrLn ("SyntaxError: " ++ show e)
    Right ast -> putStrLn $ show (generateIR ast)


parseArgs :: Parser Cmd
parseArgs = Cmd <$>
  argument str (metavar "<file>")
