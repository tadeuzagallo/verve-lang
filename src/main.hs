module Verve where

import Parser

main =
  do c <- getContents
     case parseString c of
       Left e -> do
         putStrLn "Error parsing input:"
         print e
       Right r -> print r
