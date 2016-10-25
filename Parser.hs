module Parser (Parser.parse) where

import AST
import Lexer

import Text.Parsec
import Text.Parsec.String

parse :: String -> IO (Either ParseError AST)
parse filename =
  parseFromFile p_program filename

p_program =
  AProgram <$> (many p_decl)

p_decl = DExpr <$> p_expr

p_expr = ELiteral <$> p_literal

p_literal = LNum <$> naturalOrFloat
