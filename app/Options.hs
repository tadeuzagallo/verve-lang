{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import System.Console.CmdArgs (Data, Typeable)

data Options = Options
  { dump_ir :: Bool
  , dump_statements :: Bool
  , dump_bytecode :: Bool
  , dump_serialized_bytecode :: Maybe String
  , files :: [FilePath]
  } deriving (Show, Data, Typeable)
