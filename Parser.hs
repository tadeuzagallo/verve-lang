module Parser (Parser.parse) where

import AST
import Lexer

import Text.Parsec (ParseError, eof, many, string, try, (<|>))
import Text.Parsec.String (parseFromFile)

parse :: String -> IO (Either ParseError AST)
parse filename =
  parseFromFile p_program filename

p_program =
  AProgram <$> (many p_decl) <* eof

p_decl = DBind <$> p_bind

p_bind = 
      BLet <$> ((reserved "let") *> identifier) <*> (char '=' *> p_stmt)
  <|> BFn <$> ((reserved "fn") *> identifier) <*> p_fn
  <|> BStmt <$> p_stmt


p_stmt = SExpr <$>  p_expr

p_expr = exprParser p_expr'

p_expr' =
  (EFn <$> (reserved "fn" *> p_fn)
  <|> ELiteral <$> p_literal
  <|> EVar <$> identifier) >>= p_call

p_call expr = do
  ((ECall expr <$> parens (list p_expr)) >>= p_call) <|> return expr

p_fn =
  Fn <$> parens (list p_fn_param)
     <*> (string "->" *> p_type)
     <*> braces (many p_bind)

p_fn_param = identifier

p_type = TBasic <$> identifier

p_literal =
  LNum <$> naturalOrFloat
  <|> LStr <$> stringLiteral
