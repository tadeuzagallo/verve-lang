{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  , parseStmt
  ) where

import Absyn.Untyped
import Error
import Lexer

import Text.Parsec ((<|>), (<?>), choice, eof, option, optional, parse, try, optionMaybe, sepBy1, sepBy, sepEndBy, endBy, skipMany1, lookAhead, many)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either [Error] Module)
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . (:[]) . Error) Right result

parseStmt :: String -> String -> Result Stmt
parseStmt file source = liftError $ parse (anySpace *> p_stmt <* eof) file source

p_module :: Parser Module
p_module = do
  anySpace
  imports <- p_import `sepEndBy` p_separator
  stmts <- p_stmt `sepEndBy` p_separator
  anySpace
  eof
  return $ Module { imports, stmts }

p_import :: Parser Import
p_import = do
  iGlobal <- option False (reserved "global" >> return True)
  reserved "import"
  iModule <- ucid `sepBy` symbol "."
  iAlias <- optionMaybe (reserved "as" >> ucid)
  iItems <- optionMaybe p_importItems
  return $ Import { iGlobal, iModule, iAlias, iItems }

p_importItems :: Parser [ImportItem]
p_importItems =
  braces . commaSep $ p_importItem

p_importItem :: Parser ImportItem
p_importItem = do
  choice [ lcid >>= return . ImportValue
         , ucid >>= (\t -> ImportType t <$> parens (commaSep ucid))
         ]

p_stmt :: Parser Stmt
p_stmt = choice [ p_enum
                , p_operator
                , p_let
                , p_class
                , p_interface
                , p_implementation
                , p_typeAlias
                , p_function >>= return . FnStmt
                , p_expr True >>= return . Expr
                ] <?> "statement"

p_enum :: Parser Stmt
p_enum = do
  reserved "enum"
  name <- ucid
  generics <- option [] (angles . commaSep $ ucid)
  ctors <- p_body p_constructor
  return $ Enum name generics ctors

p_constructor :: Parser (DataCtor)
p_constructor = do
  name <- ucid
  args <- optionMaybe . parens . commaSep $ p_type
  return $ (name, args)

p_operator :: Parser Stmt
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

p_let :: Parser Stmt
p_let = do
  reserved "let"
  name <- lcid
  symbol "="
  expr <- p_expr True
  return $ Let name expr

p_class :: Parser Stmt
p_class = do
  reserved "class"
  className <- ucid
  (classVars, classMethods) <- braces $ do
    vars <- option [] $ (:) <$> p_classVar <*> many (try $ p_separator *> p_classVar)
    fns <- if null vars
              then p_function `sepEndBy` p_separator
              else many (try $ p_separator *> p_function)
    return (vars, fns)
  return $ Class { className, classVars, classMethods }

p_classVar :: Parser (Name, Type)
p_classVar = do
  reserved "let"
  p_typedName

p_interface :: Parser Stmt
p_interface = do
  reserved "interface"
  intfName <- ucid
  intfParam <- angles ucid
  intfMethods <- p_body p_fnDecl
  return $ Interface { intfName, intfParam, intfMethods }

p_fnDecl :: Parser FunctionDecl
p_fnDecl = do
  reserved "fn"
  fnDeclName <- lcid
  fnDeclGenerics <- option [] p_generics
  fnDeclParams <- parens $ commaSep p_typedName
  fnDeclRetType <- option TVoid p_retType
  return $ FunctionDecl {fnDeclName, fnDeclGenerics, fnDeclParams, fnDeclRetType}

p_implementation :: Parser Stmt
p_implementation = do
  reserved "implementation"
  implGenerics <- option [] p_generics
  implName <- ucid
  implType <- angles p_type
  implMethods <- p_body p_function
  return $ Implementation { implName, implGenerics, implType, implMethods }

p_typeAlias :: Parser Stmt
p_typeAlias = do
  reserved "type"
  aliasName <- ucid
  aliasVars <- option [] (angles . commaSep $ ucid)
  symbol "="
  aliasType <- p_type
  return TypeAlias { aliasName, aliasVars, aliasType }

p_function :: Parser Function
p_function = do
  FunctionDecl { fnDeclName = name
               , fnDeclGenerics = generics
               , fnDeclParams = params
               , fnDeclRetType = retType
               } <- p_fnDecl
  body <- p_codeBlock
  return $ Function {name, generics, params, retType, body}

p_generics :: Parser [(Name, [String])]
p_generics = angles $ commaSep p_genericParam

p_genericParam :: Parser (Name, [String])
p_genericParam = do
  var <- ucid
  bounds <- option [] (symbol ":" *>  (parens (commaSep1 ucid) <|> (ucid >>= return . (:[]) )))
  return (var, bounds)

p_typedName :: Parser Param
p_typedName = do
  name <- lcid
  symbol ":"
  ty <- p_type
  return (name, ty)

p_retType :: Parser Type
p_retType = do
  reservedOp "->"
  p_type

p_type :: Parser Type
p_type = choice [p_simpleType, p_typeArrow, p_typeRecord]

p_simpleType :: Parser Type
p_simpleType = ucid >>= p_typeApp . TName

p_typeApp :: Type -> Parser Type
p_typeApp ty = do
  angles (commaSep p_type) >>= return . TApp ty
  <|> return ty

p_typeArrow :: Parser Type
p_typeArrow = do
  tyArgs <- parens $ commaSep p_type
  reservedOp "->"
  retType <- p_type
  return $ TArrow tyArgs retType

p_typeRecord :: Parser Type
p_typeRecord = do
  fields <- braces $ commaSep ((,) <$> lcid <* symbol ":" <*> p_type)
  return $ TRecord fields

p_expr :: Bool -> Parser Expr
p_expr allowCtor = choice [ p_match
                          , p_if
                          , p_function >>= return . FnExpr
                          , p_lhs allowCtor >>= p_rhs
                          ]

p_lhs :: Bool -> Parser Expr
p_lhs allowCtor = choice [ p_record
                         , p_list
                         , p_literal >>= return . Literal
                         , try p_lcid
                         , p_ucid allowCtor
                         , parens p_parenthesizedExpr
                         ]

p_lcid :: Parser Expr
p_lcid = do
  prefix <- ucid `endBy` symbol "."
  var <- lcid
  return $ Ident (prefix ++ [var]) TPlaceholder

p_ucid :: Bool -> Parser Expr
p_ucid allowCtor = do
  parts <- ucid `sepBy1` symbol "."
  let ident = Ident parts TPlaceholder
  if allowCtor
     then p_ctor ident <|> return ident
     else return ident

p_parenthesizedExpr :: Parser Expr
p_parenthesizedExpr = do
  choice [ ParenthesizedExpr <$> p_expr True
         , operator >>= return . flip Ident TPlaceholder . (:[])
         ]

p_ctor :: Expr -> Parser Expr
p_ctor ctor = do
  r <- p_record
  return (Call ctor [] [] [r])

p_rhs :: Expr -> Parser Expr
p_rhs lhs = (choice [try $ p_call lhs, p_fieldAccess lhs, p_binop lhs] >>= p_rhs) <|> return lhs

p_record :: Parser Expr
p_record =
  Record <$> braces (commaSep field)
    where
      field = (,) <$> lcid <*> (symbol ":" *> p_expr True)

p_list :: Parser Expr
p_list =
  List TPlaceholder <$> (brackets . commaSep $ p_expr True)

p_call :: Expr -> Parser Expr
p_call callee = do
  (typeArgs, args) <- p_callArgs
  return $ Call {callee, constraintArgs = [], typeArgs, args}

p_binop :: Expr -> Parser Expr
p_binop lhs = do
  op <- operator
  rhs <- p_expr True
  return $ BinOp {opConstraintArgs = [], opTypeArgs = [], lhs, op, rhs}

p_fieldAccess :: Expr -> Parser Expr
p_fieldAccess lhs = do
  symbol "."
  fieldName <- lcid
  p_methodCall lhs fieldName <|> return (FieldAccess lhs TPlaceholder fieldName)

p_methodCall :: Expr -> Name -> Parser Expr
p_methodCall lhs name = do
  (typeArgs, args) <- p_callArgs
  return $ Call { callee = Ident [name] TPlaceholder, constraintArgs = [], typeArgs, args = lhs : args }

p_callArgs :: Parser ([Type], [Expr])
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

p_match :: Parser Expr
p_match = do
  reserved "match"
  expr <- p_expr False
  cases <- p_body p_case
  return $ Match { expr, cases }

p_case :: Parser Case
p_case = do
  reserved "case"
  pattern <- p_pattern
  symbol ":"
  caseBody <- p_caseBody True
  return $ Case { pattern, caseBody }

p_caseBody :: Bool -> Parser [Stmt]
p_caseBody first = do
  (lookAheadCase >> return [])
  <|>
    (option [] ((:) <$> (try $ sep >> p_stmt) <*> p_caseBody False))
    where
      lookAheadCase = lookAhead . try $ optional p_separator >> reserved "case"
      sep
        | first = optional p_separator
        | otherwise = p_separator

p_pattern :: Parser Pattern
p_pattern = choice [ symbol "_" >> return PatDefault
                   , p_literal >>= return . PatLiteral
                   , p_patRecord
                   , p_patList
                   , p_patCtor >>= return . uncurry PatCtor
                   , lcid >>= return . PatVar
                   ]

p_patRecord :: Parser Pattern
p_patRecord =
  let field = do
        key <- lcid
        symbol ":"
        value <- p_pattern
        return (key, value)
   in PatRecord <$> braces (commaSep field)

p_patList :: Parser Pattern
p_patList = do
  let fields =
        option ([], NoRest) (field <|> rest)

      field = do
        pat <- p_pattern
        (pats, rest) <- option ([], NoRest) (comma *> fields)
        return (pat : pats, rest)

      rest = do
        symbol "..."
        rest <- option DiscardRest (NamedRest <$> lcid)
        return ([], rest)

   in uncurry PatList <$> brackets fields

p_patCtor :: Parser (Name, [Pattern])
p_patCtor = do
  ctorName <- ucid
  vars <- option [] . parens . commaSep $ p_pattern
  return (ctorName, vars)

p_if :: Parser Expr
p_if = do
  reserved "if"
  ifCond <- p_expr False
  ifBody <- p_codeBlock
  ifElseBody <- option [] p_else
  return $ If { ifCond, ifBody, ifElseBody }

p_else :: Parser [Stmt]
p_else = do
  reserved "else"
  p_codeBlock <|> ((:[]) <$> (Expr <$> p_if))

p_codeBlock :: Parser [Stmt]
p_codeBlock = p_body p_stmt

p_body :: Parser a -> Parser [a]
p_body p = braces $ p `sepEndBy` p_separator

p_separator :: Parser ()
p_separator = skipMany1 newline
