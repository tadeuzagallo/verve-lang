module Parser (Parser.parse) where

import AST
import Lexer

import Text.Parsec (ParseError, eof, many, string, (<|>))
import Text.Parsec.String (parseFromFile)

parse :: String -> IO (Either ParseError AST)
parse filename =
  parseFromFile p_program filename

p_program =
  AProgram <$> (many p_decl) <* eof

p_decl = DExpr <$> p_expr

p_expr = exprParser p_expr'

p_expr' =
      EFn <$> p_fn
  <|> ELiteral <$> p_literal

p_fn =
  Fn <$> (char '\\' *> parens (list p_fn_param))
     <*> (string "->" *> p_type)
     <*> braces (many p_expr)

p_fn_param = identifier

p_type = TBasic <$> identifier

p_literal = LNum <$> naturalOrFloat
