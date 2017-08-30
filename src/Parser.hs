{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  , parseStmt
  ) where

import Absyn
import Error
import Lexer

import Text.Parsec ((<|>), (<?>), choice, eof, option, optional, parse, try, optionMaybe, sepEndBy, skipMany1, lookAhead)
import Text.Parsec.String (Parser, parseFromFile)

type AbsynParser a = Parser (a Name UnresolvedType)

parseFile :: String -> IO (Either Error (Module Name UnresolvedType))
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . Error) Right result

parseStmt :: String -> String -> Either Error (Stmt Name UnresolvedType)
parseStmt file source = liftError $ parse (anySpace *> p_stmt <* eof) file source

p_module :: AbsynParser Module
p_module = Module <$> (p_stmt `sepEndBy` p_separator <* eof)

p_stmt :: AbsynParser Stmt
p_stmt = choice [ p_enum
                , p_operator
                , p_let
                , p_class
                , p_interface
                , p_implementation
                , p_function >>= return . FnStmt
                , p_expr True >>= return . Expr
                ] <?> "statement"

p_enum :: AbsynParser Stmt
p_enum = do
  reserved "enum"
  name <- ucid
  generics <- option [] (angles . commaSep $ ucid)
  ctors <- p_body p_constructor
  return $ Enum name generics ctors

p_constructor :: Parser (DataCtor Name UnresolvedType)
p_constructor = do
  name <- ucid
  args <- optionMaybe . parens . commaSep $ p_type
  return $ (name, args)

p_operator :: AbsynParser Stmt
p_operator = do
  maybeOpAssoc <- optionMaybe p_opAssoc
  opPrec <- option defaultPrec p_opPrec
  opAssoc <- case maybeOpAssoc of
               Just assoc -> return assoc
               Nothing -> option defaultAssoc p_opAssoc
  anySpace
  reserved "operator"
  opGenerics <- option [] p_generics
  opLhs <- parens p_typedName
  opName <- operator
  opRhs <- parens p_typedName
  opRetType <- p_retType
  opBody <- p_codeBlock
  return $ Operator { opPrec
                    , opAssoc
                    , opGenerics
                    , opLhs
                    , opName
                    , opRhs
                    , opRetType
                    , opBody
                    }

p_opPrec :: Parser Precedence
p_opPrec = do
  try $ reserved "#prec"
  parens $ choice [ relative "higher" PrecHigher
                  , relative "lower" PrecLower
                  , relative "equal" PrecEqual
                  , natural >>= return . PrecValue
                  ]
  where
    relative name fn = do
      try (reserved name)
      fn <$> parens operator

p_opAssoc :: Parser Associativity
p_opAssoc = do
  try $ reserved "#assoc"
  parens $ assocChoice [AssocLeft, AssocRight, AssocNone]
    where
      assocChoice = choice . map assoc
      assoc a = try (reserved $ show a) *> return a

p_let :: AbsynParser Stmt
p_let = do
  reserved "let"
  name <- lcid
  symbol "="
  expr <- p_expr True
  return $ Let name expr

p_class :: AbsynParser Stmt
p_class = do
  reserved "class"
  className <- ucid
  (classVars, classMethods) <-
    braces $ (,)
    <$> p_classVar `sepEndBy` p_separator
    <*> p_function `sepEndBy` p_separator
  return $ Class { className, classVars, classMethods }

p_classVar :: Parser (Name, UnresolvedType)
p_classVar = do
  reserved "let"
  p_typedName

p_interface :: AbsynParser Stmt
p_interface = do
  reserved "interface"
  intfName <- ucid
  intfParam <- angles ucid
  intfMethods <- p_body p_fnDecl
  return $ Interface { intfName, intfParam, intfMethods }

p_fnDecl :: AbsynParser FunctionDecl
p_fnDecl = do
  reserved "fn"
  fnDeclName <- lcid
  fnDeclGenerics <- option [] p_generics
  fnDeclParams <- parens $ commaSep p_typedName
  fnDeclRetType <- option UTVoid p_retType
  return $ FunctionDecl {fnDeclName, fnDeclGenerics, fnDeclParams, fnDeclRetType}

p_implementation :: AbsynParser Stmt
p_implementation = do
  reserved "implementation"
  implName <- ucid
  implType <- angles p_type
  implMethods <- p_body p_function
  return $ Implementation { implName, implType, implMethods }

p_function :: AbsynParser Function
p_function = do
  FunctionDecl { fnDeclName = name
               , fnDeclGenerics = generics
               , fnDeclParams = params
               , fnDeclRetType = retType
               } <- p_fnDecl
  body <- p_codeBlock
  return $ Function {name, generics, params, retType, body}

p_generics :: Parser [(Name, [UnresolvedType])]
p_generics = angles $ commaSep p_genericParam

p_genericParam :: Parser (Name, [UnresolvedType])
p_genericParam = do
  var <- ucid
  bounds <- option ([UTTop]) (symbol ":" *>  (parens (commaSep1 p_type) <|> (p_type >>= return . (:[]) )))
  return (var, bounds)

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
p_type = choice [p_simpleType, p_typeArrow, p_typeRecord]

p_simpleType :: Parser UnresolvedType
p_simpleType = ucid >>= p_typeApp . UTName

p_typeApp :: UnresolvedType -> Parser UnresolvedType
p_typeApp ty = do
  angles (commaSep p_type) >>= return . UTApp ty
  <|> return ty

p_typeArrow :: Parser UnresolvedType
p_typeArrow = do
  tyArgs <- parens $ commaSep p_type
  reservedOp "->"
  retType <- p_type
  return $ UTArrow tyArgs retType

p_typeRecord :: Parser UnresolvedType
p_typeRecord = do
  fields <- braces $ commaSep ((,) <$> lcid <* symbol ":" <*> p_type)
  return $ UTRecord fields

p_expr :: Bool -> AbsynParser Expr
p_expr allowCtor = choice [ p_match
                          , p_if
                          , p_lhs allowCtor >>= p_rhs
                          ]

p_lhs :: Bool -> AbsynParser Expr
p_lhs allowCtor = choice [ p_record
                         , p_list
                         , p_literal >>= return . Literal
                         , lcid >>= return . Ident
                         , p_ucidCtor allowCtor
                         , parens (p_expr True <|> (operator >>= return . Ident))
                         ]

p_ucidCtor :: Bool -> AbsynParser Expr
p_ucidCtor allowCtor = do
  name <- ucid
  let name' = Ident name
  if allowCtor
     then p_ctor name <|> return name'
     else return name'

p_ctor :: Name -> AbsynParser Expr
p_ctor name = do
  r <- p_record
  return (Call (Ident name) [] [] [r])

p_rhs :: Expr Name UnresolvedType -> AbsynParser Expr
p_rhs lhs = (choice [try $ p_call lhs, p_fieldAccess lhs, p_binop lhs] >>= p_rhs) <|> return lhs

p_record :: AbsynParser Expr
p_record =
  Record <$> braces (commaSep field)
    where
      field = (,) <$> lcid <*> (symbol ":" *> p_expr True)

p_list :: AbsynParser Expr
p_list =
  List <$> (brackets . commaSep $ p_expr True)

p_call :: Expr Name UnresolvedType -> AbsynParser Expr
p_call callee = do
  (typeArgs, args) <- p_callArgs
  return $ Call {callee, constraintArgs = [], typeArgs, args}

p_binop :: Expr Name UnresolvedType -> AbsynParser Expr
p_binop lhs = do
  op <- operator
  rhs <- p_expr True
  return $ BinOp {opTypeArgs = [], lhs, op, rhs}

p_fieldAccess :: Expr Name UnresolvedType -> AbsynParser Expr
p_fieldAccess lhs = do
  symbol "."
  fieldName <- lcid
  p_methodCall lhs fieldName <|> return (FieldAccess lhs UTPlaceholder fieldName)

p_methodCall :: Expr Name UnresolvedType -> Name -> AbsynParser Expr
p_methodCall lhs name = do
  (typeArgs, args) <- p_callArgs
  return $ Call { callee = Ident name, constraintArgs = [], typeArgs, args = lhs : args }

p_callArgs :: Parser ([UnresolvedType], [Expr Name UnresolvedType])
p_callArgs = do
  typeArgs <- option [] $ angles (commaSep $ p_type)
  args <- parens $ commaSep (p_expr True)
  return (typeArgs, args)

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

p_match :: AbsynParser Expr
p_match = do
  reserved "match"
  expr <- p_expr False
  cases <- p_body p_case
  return $ Match { expr, cases }

p_case :: AbsynParser Case
p_case = do
  reserved "case"
  pattern <- p_pattern
  symbol ":"
  caseBody <- p_caseBody True
  return $ Case { pattern, caseBody }

p_caseBody :: Bool -> Parser [Stmt Name UnresolvedType]
p_caseBody first = do
  (lookAheadCase >> return [])
  <|>
    (option [] ((:) <$> (try $ sep >> p_stmt) <*> p_caseBody False))
    where
      lookAheadCase = lookAhead . try $ optional p_separator >> reserved "case"
      sep
        | first = optional p_separator
        | otherwise = p_separator

p_pattern :: Parser (Pattern Name)
p_pattern = choice [ symbol "_" >> return PatDefault
                   , p_literal >>= return . PatLiteral
                   , p_patCtor >>= return . uncurry PatCtor
                   , lcid >>= return . PatVar
                   ]

p_patCtor :: Parser (Name, [Pattern Name])
p_patCtor = do
  ctorName <- ucid
  vars <- option [] . parens . commaSep $ p_pattern
  return (ctorName, vars)

p_if :: AbsynParser Expr
p_if = do
  reserved "if"
  ifCond <- p_expr False
  ifBody <- p_codeBlock
  ifElseBody <- option [] p_else
  return $ If { ifCond, ifBody, ifElseBody }

p_else :: Parser [Stmt Name UnresolvedType]
p_else = do
  reserved "else"
  p_codeBlock <|> ((:[]) <$> (Expr <$> p_if))

p_codeBlock :: Parser [Stmt Name UnresolvedType]
p_codeBlock = p_body p_stmt

p_body :: Parser a -> Parser [a]
p_body p = braces $ p `sepEndBy` p_separator

p_separator :: Parser ()
p_separator = skipMany1 newline
