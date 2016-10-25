module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data AST = AProgram [Decl]
  deriving (Show)

data Decl =
  DExpr Expr
  deriving (Show)

data Expr =
  ELiteral Literal
  deriving (Show)

data Literal =
  LNum (Either Integer Double)
  deriving (Show)

lexer = makeTokenParser javaStyle

parse :: String -> IO (Either ParseError AST)
parse filename =
  parseFromFile p_program filename

p_program =
  AProgram <$> (many p_decl)

p_decl = DExpr <$> p_expr

p_expr = ELiteral <$> p_literal

p_literal = LNum <$> naturalOrFloat lexer
