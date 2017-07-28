{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  ) where

import Absyn
import Lexer

import Text.Parsec (ParseError, (<|>), choice, eof, try)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either ParseError Expr)
parseFile = parseFromFile p_program

p_program :: Parser Expr
p_program = p_expr <* eof

p_expr :: Parser Expr
p_expr = do
  lhs <- p_lhs
  p_rhs lhs <|> return lhs

p_lhs :: Parser Expr
p_lhs = p_literal >>= return . Literal

p_rhs :: Expr -> Parser Expr
p_rhs lhs = do
  op <- try operator
  rhs <- p_expr
  return $ BinOp {lhs, op, rhs}

p_literal :: Parser Literal
p_literal =
  choice
    [ integer >>= return . Integer
    , float >>= return . Float
    , charLiteral >>= return . Char
    , stringLiteral >>= return . String
    ]
