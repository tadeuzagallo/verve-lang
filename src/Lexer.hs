module Lexer (integer
             , float
             , stringLiteral
             , charLiteral
             -- puctuation
             , parens
             , braces
             ) where

import Text.Parsec.Language (javaStyle)

import qualified Text.Parsec.Token as Token

lexer = Token.makeTokenParser javaStyle

-- Literals
integer = Token.integer lexer
float = Token.float lexer
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer

parens = Token.parens lexer
braces = Token.braces lexer

-- Keywords
identifier = Token.identifier lexer
reserved = Token.reserved lexer
operator = Token.operator lexer
