module Lexer
  -- Literals
  ( naturalOrFloat
  , stringLiteral
  , charLiteral
  -- Keywords
  , identifier
  , reserved
  , operator
  , symbol
  -- Delimiters
  , parens
  , braces
  -- Utils
  , commaSep
  ) where

import Text.Parsec (sepEndBy)
import Text.Parsec.Language (javaStyle)

import qualified Text.Parsec.Token as Token

lexer = Token.makeTokenParser javaStyle

-- Literals
naturalOrFloat = Token.naturalOrFloat lexer

stringLiteral = Token.stringLiteral lexer

charLiteral = Token.charLiteral lexer

-- Delimiters
parens = Token.parens lexer

braces = Token.braces lexer

comma = Token.comma lexer

-- Keywords
identifier = Token.identifier lexer

reserved = Token.reserved lexer

operator = Token.operator lexer

symbol = Token.symbol lexer

-- Utils
commaSep = flip sepEndBy comma
