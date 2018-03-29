module Syntax.Decl (p_decl) where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Expr
import Syntax.Shared
import Syntax.Type

import Text.Parsec ((<|>), choice, many, option, optionMaybe, sepEndBy, try)
import Text.Parsec.String (Parser)

p_decl :: Parser Decl
p_decl = choice [ p_enum
                , p_operator
                , p_let
                , p_class
                , p_interface
                , p_implementation
                , p_typeAlias
                , p_function >>= return . FnStmt
                ]

p_enum :: Parser Decl
p_enum = do
  reserved "enum"
  name <- ucid
  generics <- option [] (angles . commaSep $ ucid)
  ctors <- block p_constructor
  return $ Enum name generics ctors

p_constructor :: Parser (DataCtor)
p_constructor = do
  name <- ucid
  args <- optionMaybe . parens . commaSep $ p_type
  return $ (name, args)

p_operator :: Parser Decl
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
  opRetType <- option TVoid p_retType
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

p_let :: Parser Decl
p_let = do
  reserved "let"
  name <- lcid
  ty <- option TPlaceholder (symbol ":" *> p_type)
  symbol "="
  expr <- p_expr True
  return $ Let (name, ty) expr

p_class :: Parser Decl
p_class = do
  reserved "class"
  className <- ucid
  (classVars, classMethods) <- braces $ do
    vars <- option [] $ (:) <$> p_varDecl <*> many (try $ separator *> p_varDecl)
    fns <- if null vars
              then p_function `sepEndBy` separator
              else many (try $ separator *> p_function)
    return (vars, fns)
  return $ Class { className, classVars, classMethods }

p_varDecl :: Parser (Name, Type)
p_varDecl = do
  reserved "let"
  p_typedName

p_interface :: Parser Decl
p_interface = do
  reserved "interface"
  intfName <- ucid
  intfParam <- angles ucid
  intfMethods <- block (p_intfVar <|> p_intfOperator)
  return $ Interface { intfName, intfParam, intfMethods }

p_intfVar :: Parser InterfaceItem
p_intfVar = do
  reserved "let"
  IntfVar <$> p_typedName

p_intfOperator :: Parser InterfaceItem
p_intfOperator = do
  maybeOpAssoc <- optionMaybe p_opAssoc
  intfOpPrec <- option defaultPrec p_opPrec
  intfOpAssoc <- case maybeOpAssoc of
                   Just assoc -> return assoc
                   Nothing -> option defaultAssoc p_opAssoc
  anySpace
  reserved "operator"
  intfOpLhs <- parens p_type
  intfOpName <- operator
  intfOpRhs <- parens p_type
  intfOpRetType <- option TVoid p_retType
  return $ IntfOperator { intfOpPrec
                        , intfOpAssoc
                        , intfOpLhs
                        , intfOpName
                        , intfOpRhs
                        , intfOpRetType
                        }

p_implementation :: Parser Decl
p_implementation = do
  reserved "implementation"
  implGenerics <- option [] p_generics
  implIntf <- ucid
  implType <- angles p_type
  implMethods <- block p_implementationItem
  return $ Implementation { implIntf, implGenerics, implType, implMethods }

p_implementationItem :: Parser ImplementationItem
p_implementationItem = choice [ p_implFn
                              , p_implOperator
                              , p_implVar
                              ]

p_implFn :: Parser ImplementationItem
p_implFn = do
  reserved "fn"
  implName <- lcid
  implParams <- parens $ commaSep lcid
  implBody <- p_codeBlock
  return $ ImplFunction { implName, implParams, implBody }

p_implOperator :: Parser ImplementationItem
p_implOperator = do
  reserved "operator"
  implOpLhs <- lcid
  implOpName <- operator
  implOpRhs <- lcid
  implOpBody <- p_codeBlock
  return $ ImplOperator { implOpLhs, implOpName, implOpRhs, implOpBody }

p_implVar :: Parser ImplementationItem
p_implVar = do
  reserved "let"
  name <- lcid
  symbol "="
  expr <- p_expr True
  return $ ImplVar (name, expr)

p_typeAlias :: Parser Decl
p_typeAlias = do
  reserved "type"
  aliasName <- ucid
  aliasVars <- option [] (angles . commaSep $ ucid)
  symbol "="
  aliasType <- p_type
  return TypeAlias { aliasName, aliasVars, aliasType }
