module Syntax.Literal (p_literal) where

import Absyn.Untyped
import Syntax.Lexer

import Text.Parsec (choice)
import Text.Parsec.String (Parser)

p_literal :: Parser Literal
p_literal =
  choice [ p_number
         , charLiteral >>= return . Char
         , stringLiteral >>= return . String
         ]

p_number :: Parser Literal
p_number = do
  number <- naturalOrFloat
  case number of
    Left int -> return $ Integer int
    Right float -> return $ Float float


