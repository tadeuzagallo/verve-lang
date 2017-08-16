{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  , parseStmt
  ) where

import Absyn
import Error
import Lexer
import Types

import Debug.Trace

import Text.Parsec (ParseError, (<|>), choice, eof, many, many1, option, parse, try, optionMaybe, sepEndBy, skipMany1)
import Text.Parsec.String (Parser, parseFromFile)

instance ErrorT ParseError where
  kind _ = "ParseError"

parseFile :: String -> IO (Either Error (Module Name UnresolvedType))
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . Error) Right result

parseStmt :: String -> String -> Either Error (Stmt Name UnresolvedType)
parseStmt file source = liftError $ parse (p_stmt <* eof) file source

p_module :: Parser (Module Name UnresolvedType)
p_module = Module <$> (p_stmt `sepEndBy` p_separator <* eof)

p_stmt :: Parser (Stmt Name UnresolvedType)
p_stmt = choice [ p_enum
                , p_operator
                , p_let
                , p_function >>= return . FnStmt
                , p_expr >>= return . Expr
                ]

p_enum :: Parser (Stmt Name UnresolvedType)
p_enum = do
  reserved "enum"
  name <- ucid
  ctors <- p_body p_constructor
  return $ Enum name ctors

p_constructor :: Parser (DataCtor Name UnresolvedType)
p_constructor = do
  name <- ucid
  args <- optionMaybe . parens . commaSep $ p_type
  return $ (name, args)

p_operator :: Parser (Stmt Name UnresolvedType)
p_operator = do
  reserved "operator"
  opGenerics <- option [] p_generics
  opLhs <- parens p_typedName
  opName <- operator
  opRhs <- parens p_typedName
  opRetType <- p_retType
  opBody <- p_codeBlock
  return $ Operator { opGenerics
                    , opLhs
                    , opName
                    , opRhs
                    , opRetType
                    , opBody
                    }

p_let :: Parser (Stmt Name UnresolvedType)
p_let = do
  reserved "let"
  name <- lcid
  symbol "="
  expr <- p_expr
  return $ Let name expr

p_function :: Parser (Function Name UnresolvedType)
p_function = do
  reserved "fn"
  name <- lcid
  generics <- option [] p_generics
  params <- parens $ commaSep p_typedName
  retType <- option (UnresolvedType void) p_retType
  body <- p_codeBlock
  return $ Function {name, generics, params, retType, body}

p_generics :: Parser [Name]
p_generics = angles $ commaSep ucid

p_typedName :: Parser (Id UnresolvedType)
p_typedName = do
  name <- lcid
  symbol ":"
  ty <- p_type
  return (name, ty)

p_retType :: Parser UnresolvedType
p_retType = do
  reservedOp "->"
  p_type

p_type :: Parser UnresolvedType
p_type = p_type' >>= return . UnresolvedType

p_type' :: Parser Type
p_type' = choice [p_simpleType, p_typeArrow, p_typeRecord]

p_simpleType :: Parser Type
p_simpleType = ucid >>= return . Con

p_typeArrow :: Parser Type
p_typeArrow = do
  tyArgs <- parens $ commaSep p_type'
  retType <- p_type'
  return $ Fun [] tyArgs retType

p_typeRecord :: Parser Type
p_typeRecord = do
  fields <- braces $ commaSep ((,) <$> lcid <* symbol ":" <*> p_type')
  return $ Rec fields

p_expr :: Parser (Expr Name UnresolvedType)
p_expr = choice [ p_match
                , p_lhs >>= p_rhs
                ]

p_lhs :: Parser (Expr Name UnresolvedType)
p_lhs = choice [ p_record
               , p_literal >>= return . Literal
               , lcid >>= return . Ident
               , ucid >>= return . Ident
               , parens (p_expr <|> (operator >>= return . Ident))
               ]

p_rhs :: Expr Name UnresolvedType -> Parser (Expr Name UnresolvedType)
p_rhs lhs = (choice [try $ p_app lhs, p_binop lhs] >>= p_rhs) <|> return lhs

p_record :: Parser (Expr Name UnresolvedType)
p_record =
  Record <$> braces (field `sepEndBy` comma)
    where
      field = (,) <$> lcid <*> (symbol "=" *> p_expr)

p_app :: Expr Name UnresolvedType -> Parser (Expr Name UnresolvedType)
p_app callee = do
  types <- option [] $ angles (commaSep $ p_type)
  args <- parens $ commaSep p_expr
  return $ App {callee, types, args}

p_binop :: Expr Name UnresolvedType -> Parser (Expr Name UnresolvedType)
p_binop lhs = do
  op <- operator
  rhs <- p_expr
  return $ BinOp {lhs, op, rhs}

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

p_match :: Parser (Expr Name UnresolvedType)
p_match = do
  reserved "match"
  expr <- p_expr
  cases <- p_body p_case
  return $ Match { expr, cases }

p_case :: Parser (Case Name UnresolvedType)
p_case = do
  pattern <- p_pattern
  symbol ":"
  caseBody <- p_expr
  return $ Case { pattern, caseBody }

p_pattern :: Parser (Pattern Name)
p_pattern = choice [ p_literal >>= return . PatLiteral
                   , p_ctor >>= return . uncurry PatCtor
                   , symbol "_" >> return PatDefault
                   , lcid >>= return . PatVar
                   ]

p_ctor :: Parser (Name, [Pattern Name])
p_ctor = do
  ctorName <- ucid
  vars <- option [] . parens . commaSep $ p_pattern
  return (ctorName, vars)

p_codeBlock :: Parser [Stmt Name UnresolvedType]
p_codeBlock = p_body p_stmt

p_body :: Parser a -> Parser [a]
p_body p = braces $ p `sepEndBy` p_separator

p_separator :: Parser ()
p_separator = skipMany1 newline
