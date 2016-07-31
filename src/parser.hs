module Parser where

import AST
import Lexer

import Control.Monad (liftM)
import Text.Parsec hiding (string, char)

p_program = (Program <$> (many p_import) <*> (many p_decl)) <* eof

p_import = Import <$> ((try $ string "import") *> p_import_name) <*> (string "from" *> string_literal) <*>  (optionMaybe ((string "as") *> identifier))

p_import_name = (char '*' *> return Nothing) <|> (liftM Just (braces $ list identifier))

p_decl = p_interface
     <|> p_implementation
     <|> p_type_decl
     <|> p_extern
     <|> p_expr

p_interface =
  Interface <$> ((try $ string "interface") *> identifier)
    <*> angles identifier
    <*> braces (many1 (p_virtual_function <|> p_function))

p_implementation =
  Implementation <$> ((try $ string "implementation") *> identifier)
  <*> angles p_type
  <*> braces (many1 (p_extern <|> p_typeless_function))

p_extern_function =
  (try $ string "extern") *> p_prototype

p_virtual_function =
  (try $ string "virtual") *> p_prototype

p_typeless_function =
  (try $ string "fn") *>
    (Function <$> identifier
    <*> parens (list (FunctionParameter <$> identifier <*> (return Nothing)))
    <*> (return Nothing)
    <*> p_block)

p_type_decl = EnumType <$> ((try $ string "type") *> identifier) <*> p_generics <*> (braces $ many1 p_type_ctor)

p_generics = (angles $ list1 identifier)

p_type_ctor = TypeContructor <$> identifier <*> (parens $ list p_type)

p_type = p_function_type <|> try p_data_type <|> p_basic_type

p_function_type = FunctionType <$> (parens $ list p_type) <*> ((string "->") *> p_type)

p_data_type = DataType <$> identifier <*> (angles $ list1 p_type)

p_basic_type = BasicType <$> identifier

p_extern = (try $ string "extern") *> p_prototype

p_prototype = Prototype <$> identifier <*> p_generics <*> p_function_type

-- TODO: operators
p_expr = expr_parser p_factor

p_factor = p_list
       <|> p_unop
       <|> p_number
       <|> p_string
       <|> p_if
       <|> p_let
       <|> p_match
       <|> p_call
       <|> parens p_expr

p_list = List <$> brackets (list p_expr)

p_unop = UnaryOp <$> operator <*> p_expr

p_number = Number <$> naturalOrFloat

p_string = String <$> string_literal

p_if = (try $ string "if") *> (If <$> (parens p_expr <|> p_expr) <*> p_block_or_expr <*> optionMaybe ((try $ string "else") *> p_block_or_expr))

p_block_or_expr = p_block <|> p_expr

p_block = (Block <$> (braces $ many p_expr))

p_let = (try $ string "let") *>
  (Let <$> (many p_assignment) <*> p_block)

p_assignment = Assignment <$> p_identifier <*> (char '=' *> p_expr)

p_match = (try $ string "match") *>
  (Match <$> p_expr <*> p_cases)

p_cases = braces . many1 $
  Case <$> p_pattern
       <*> (string "=>" *> p_block_or_expr)

p_pattern =
  Pattern <$> identifier
          <*> parens (list identifier)

p_call = make_call <$> (p_function <|> p_identifier) <*> many (parens $ list p_expr)
  where make_call x xs = foldl Call x xs

p_function = (try $ string "fn") *>
  (Function <$> identifier
  <*> p_params
  <*> liftM Just p_ret_type
  <*> p_block)

p_params =
  parens . list $
    FunctionParameter <$> identifier
    <*> (char ':' *> liftM Just p_type)

p_ret_type = (string "->") *> p_type

p_identifier = Identifier <$> identifier

parseString :: String -> Either ParseError AST
parseString input = parse p_program "(stdin)" input
