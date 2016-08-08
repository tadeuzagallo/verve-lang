module Verve where

import Parser
import Generator

import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(WriteMode))

main = do
  args <- getArgs
  c <- getContents
  case parseString c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r ->
      withBinaryFile (args !! 0) WriteMode (generate r)
