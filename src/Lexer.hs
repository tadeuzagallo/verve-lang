module Lexer
  -- Literals
  ( naturalOrFloat
  , stringLiteral
  , charLiteral
  -- Keywords
  , lcid
  , ucid
  , reserved
  , operator
  , symbol
  -- Delimiters
  , parens
  , braces
  , angles
  -- Utils
  , commaSep
  ) where

import Text.Parsec (sepEndBy, many)
import Text.Parsec.Char (lower, upper, alphaNum)
import Text.Parsec.String (Parser)
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
angles = Token.angles lexer

comma = Token.comma lexer

-- Keywords
lcid :: Parser String
lcid = (:) <$> lower <*> many alphaNum <* whiteSpace

ucid :: Parser String
ucid = (:) <$> upper <*> many alphaNum <* whiteSpace

reserved = Token.reserved lexer
operator = Token.operator lexer
symbol = Token.symbol lexer

-- Utils
commaSep = flip sepEndBy comma
whiteSpace = Token.whiteSpace lexer
