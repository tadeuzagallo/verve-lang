module Lexer where

import AST

import Text.Parsec.Expr

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

lexer = T.makeTokenParser L.javaStyle

braces = T.braces lexer
char a = whiteSpace *> (P.char a) <* whiteSpace
exprParser = buildExpressionParser op_table
identifier = whiteSpace *> T.identifier lexer <* whiteSpace
naturalOrFloat = T.naturalOrFloat lexer
parens = T.parens lexer
reservedOp = T.reservedOp lexer
whiteSpace = T.whiteSpace lexer
stringLiteral = T.stringLiteral lexer

list = (`P.sepEndBy` (char ','))

op_table =
  [ [prefix "-", prefix "+"]
  , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft]
  , [binary "+" AssocLeft, binary "-" AssocLeft]
  , [binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft]
  , [binary "==" AssocLeft, binary "!=" AssocLeft]
  , [binary "&&" AssocLeft, binary "||" AssocLeft]
  ]
    where
      binary name assoc = Infix (do{ reservedOp name; return (EBinop name) }) assoc;
      prefix name = Prefix (do{ reservedOp name; return (EUnop name) })

