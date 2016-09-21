module Parser (parseString) where

import AST
import Lexer

import Control.Monad (liftM)
import Text.Parsec hiding (string, char, getPosition)

p_program =
  (Program <$> getPosition
           <*> (many p_import)
           <*> (many p_decl))
  <* eof

p_import =
  Import <$>  getPosition
         <*> ((try $ string "import") *> p_import_name)
         <*> (string "from" *> string_literal)
         <*>  optionMaybe ((string "as") *> identifier)

p_import_name = (char '*' *> return Nothing)
            <|> (liftM Just (braces $ list identifier))

p_decl = p_interface
     <|> p_implementation
     <|> p_type_decl
     <|> p_extern
     <|> p_expr

p_interface =
  Interface <$> getPosition
            <*> ((try $ string "interface") *> identifier)
            <*> angles identifier
            <*> braces (many1 (p_virtual_function <|> p_function))

p_implementation =
  Implementation <$> getPosition
                 <*> ((try $ string "implementation") *> identifier)
                 <*> angles p_type
                 <*> braces (many1 (p_extern <|> p_typeless_function))

p_extern = (try $ string "extern") *>
  (Extern <$> getPosition <*> p_prototype)

p_virtual_function = (try $ string "virtual") *>
  (Virtual <$> getPosition <*> p_prototype)

p_typeless_function = (try $ string "fn") *>
  (Function <$> getPosition
            <*> identifier
            <*> return Nothing
            <*> parens (list (FunctionParameter <$> getPosition <*> identifier <*> (return 0) <*> (return Nothing)))
            <*> (return Nothing)
            <*> p_block)

p_type_decl = (try $ string "type") *>
  (EnumType <$> getPosition
            <*> identifier
            <*> p_generics
            <*> (braces $ many1 p_type_ctor))

p_generics = optionMaybe . angles $ list1 identifier

p_type_ctor =
  TypeContructor <$> getPosition
                 <*> identifier
                 <*> (parens $ list p_type)

p_type = p_function_type
     <|> try p_data_type
     <|> p_basic_type

p_function_type =
  FunctionType <$> getPosition
               <*> return Nothing
               <*> (parens $ list p_type)
               <*> ((string "->") *> p_type)

p_data_type =
  DataType <$> getPosition
           <*> identifier
           <*> (angles $ list1 p_type)

p_basic_type =
  BasicType <$> getPosition
            <*> identifier

p_prototype =
  Prototype <$> getPosition
            <*> identifier
            <*> p_function_type

-- TODO: operators
p_expr = expr_parser p_factor

p_factor = p_list
       <|> p_number
       <|> p_string
       <|> p_if
       <|> p_let
       <|> p_match
       <|> p_call
       <|> parens p_expr

p_list =
  List <$> getPosition
       <*> brackets (list p_expr)

p_number =
  Number <$> getPosition
         <*> naturalOrFloat

p_string =
  String <$> getPosition
         <*> string_literal

p_if = (try $ string "if") *>
  (If <$> getPosition
      <*> p_expr
      <*> p_block_or_expr
      <*> optionMaybe ((try $ string "else") *> p_block_or_expr))

p_block_or_expr = p_block <|> p_expr

p_block =
  Block <$> getPosition
        <*> (braces $ many p_expr)

p_let = (try $ string "let") *>
  (Let <$> getPosition
       <*> (many p_assignment)
       <*> p_block)

p_assignment =
  Assignment <$> getPosition
             <*> p_identifier
             <*> (char '=' *> p_expr)

p_match = (try $ string "match") *>
  (Match <$> getPosition
         <*> p_expr
         <*> p_cases)

p_cases = braces . many1 $
  Case <$> getPosition
       <*> p_pattern
       <*> (string "=>" *> p_block_or_expr)

p_pattern =
  Pattern <$> getPosition
          <*> identifier
          <*> parens (list identifier)

p_call =
  make_call <$> getPosition
            <*> (p_function <|> p_identifier)
            <*> many (parens $ list p_expr)
    where make_call pos x xs = foldl (Call pos) x xs

p_function = (try $ string "fn") *>
  (Function <$> getPosition
            <*> identifier
            <*> p_generics
            <*> p_params
            <*> liftM Just p_ret_type
            <*> p_block)

p_params =
  parens . list $
    FunctionParameter <$> getPosition
                      <*> identifier
                      <*> (return 0)
                      <*> (char ':' *> liftM Just p_type)

p_ret_type = (string "->") *> p_type

p_identifier =
  Identifier <$> getPosition
             <*> identifier

parseString :: String -> String -> Either ParseError AST
parseString file_name source = parse p_program file_name source
