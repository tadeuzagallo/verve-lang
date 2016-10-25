import Options.Applicative

data Cmd = Cmd { file :: String }

main :: IO ()
main =
  execParser opts >>= runCmd
    where
      opts = info (helper <*> parseArgs) (fullDesc <> header "Verve compiler")

runCmd :: Cmd -> IO ()
runCmd (Cmd file) =
  putStrLn ("Compile: " ++ file)

parseArgs :: Parser Cmd
parseArgs = Cmd <$>
  argument str (metavar "<file>")
