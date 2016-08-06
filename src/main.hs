module Verve where

import Parser
import Generator

main =
  do c <- getContents
     case parseString c of
       Left e -> do
         putStrLn "Error parsing input:"
         print e
       Right r -> print (generate r)
