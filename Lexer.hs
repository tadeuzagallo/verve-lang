module Lexer where

import AST

import Text.Parsec.Expr

import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

lexer = T.makeTokenParser L.javaStyle

naturalOrFloat = T.naturalOrFloat lexer
exprParser = buildExpressionParser op_table
reservedOp = T.reservedOp lexer

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

