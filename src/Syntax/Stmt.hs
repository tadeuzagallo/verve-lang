module Syntax.Stmt
  ( p_stmt
  , p_stmts
  ) where

import Absyn.Untyped
import Syntax.Decl
import Syntax.Expr
import Syntax.Lexer
import Syntax.Shared

import Text.Parsec ((<?>), choice, sepEndBy)
import Text.Parsec.String (Parser)

p_stmts :: Parser [Stmt]
p_stmts =
  p_stmt `sepEndBy` separator

p_stmt :: Parser Stmt
p_stmt = choice [ liftParser $ Decl <$> p_decl
                , liftParser $ Expr <$> p_expr True
                ] <?> "statement"
