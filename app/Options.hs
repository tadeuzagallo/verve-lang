{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import System.Console.CmdArgs (Data, Typeable)

data Options = Options
  { dump_ir :: Bool
  , dump_statements :: Bool
  , files :: [FilePath]
  } deriving (Show, Data, Typeable)
