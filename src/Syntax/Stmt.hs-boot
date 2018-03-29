module Syntax.Stmt where

import Absyn.Untyped

import Text.Parsec.String (Parser)

p_stmt :: Parser Stmt
p_stmts :: Parser [Stmt]
