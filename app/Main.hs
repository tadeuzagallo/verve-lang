module Main where

import Compile
import Options
import Repl
import Runners

import System.Console.CmdArgs

options :: Options
options
  = Options { dump_ir = def &= help "Dump the intermediate representation of the program"
            , dump_statements = def &= help "Dump the value and type of each statement in the program"
            , files = def &= args &= typFile
            }

main :: IO ()
main = do
  args <- cmdArgs options
  if null (files args) then runPipeline repl args else
    if dump_statements args
       then runPipeline (evalProgram runEach) args
       else runPipeline (evalProgram runAll) args
