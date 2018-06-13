module Syntax.Parser
  ( parseFile
  , parseStmt
  ) where

import Syntax.Import
import Syntax.Lexer
import Syntax.Shared
import Syntax.Stmt

import Absyn.Untyped
import Util.Error

import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either [Error] Module)
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . (:[]) . Error) Right result

parseStmt :: String -> String -> Result Stmt
parseStmt file source = liftParseError $ parse (anySpace *> p_stmt <* eof) file source

p_module :: Parser Module
p_module = liftParser $ do
  anySpace
  imports <- p_imports
  stmts <- p_stmts
  anySpace
  eof
  return $ Module { imports, stmts }
