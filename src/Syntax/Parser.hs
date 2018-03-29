module Syntax.Parser
  ( parseFile
  , parseStmt
  ) where

import Syntax.Import
import Syntax.Stmt

import Absyn.Untyped
import Syntax.Lexer
import Util.Error

import Text.Parsec (eof, getPosition, parse, sourceName, sourceLine, sourceColumn)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either [Error] Module)
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . (:[]) . Error) Right result

parseStmt :: String -> String -> Result Stmt
parseStmt file source = liftError $ parse (anySpace *> p_stmt <* eof) file source

p_module :: Parser Module
p_module = do
  anySpace
  imports <- p_imports
  stmts <- p_stmts
  anySpace
  eof
  return $ Module { imports, stmts }


_located :: Parser a -> Parser (Located a)
_located p = do
  start <- getPosition
  ast <- p
  end <- getPosition
  let l = SourceLoc { file = sourceName start
                   , start = SourcePos { line = sourceLine start
                                       , column = sourceColumn start
                                       }
                   , end = SourcePos { line = sourceLine end
                                     , column = sourceColumn end
                                     }
                   }
  return $ L l ast
