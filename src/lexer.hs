module Lexer where

import AST

import Control.Monad (mplus)
import Text.Parsec.Expr
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as Parsec

lexer = Token.makeTokenParser javaStyle {
  Token.identStart = Parsec.letter `mplus` Parsec.char '_'
}

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operator = Token.operator lexer
naturalOrFloat = Token.naturalOrFloat lexer
parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
angles = Token.angles lexer
string_literal = whiteSpace *> (Token.stringLiteral lexer) <* whiteSpace

list = (`Parsec.sepEndBy` (char ','))
list1 = (`Parsec.sepEndBy1` (char ','))

whiteSpace = Token.whiteSpace lexer
string a = whiteSpace *> (Parsec.string a) <* whiteSpace
char a = whiteSpace *> (Parsec.char a) <* whiteSpace

op_table =
  [ [prefix "-", prefix "+"]
  , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft]
  , [binary "+" AssocLeft, binary "-" AssocLeft]
  , [binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft]
  , [binary "==" AssocLeft, binary "!=" AssocLeft]
  , [binary "&&" AssocLeft, binary "||" AssocLeft]
  ]

binary  name assoc = Infix (do{ reservedOp name; return (BinaryOp name) }) assoc
prefix  name = Prefix (do{ reservedOp name; return (UnaryOp name) })

expr_parser = buildExpressionParser op_table
