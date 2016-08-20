module Naming (naming) where

import AST

import qualified Data.Map as Map

type Context = (Map.Map String AST)

naming :: AST -> AST
naming = naming' Map.empty

naming' :: Context -> AST -> AST
naming' ctx ast =
  case ast of
    Program imports nodes -> Program imports (naming' ctx <$> nodes)
    Block nodes -> Block (naming' ctx <$> nodes)
    List items -> List (naming' ctx <$> items)
    UnaryOp op operand -> UnaryOp op (naming' ctx operand)
    BinaryOp op lhs rhs -> BinaryOp op (naming' ctx lhs) (naming' ctx rhs)
    If cond conseq alt -> If (naming' ctx cond) (naming' ctx conseq) (naming' ctx <$> alt)
    Function name params ret_type body ->
      let params' = foldl (\ params p -> let (FunctionParameter name _ t) = p in params ++ [FunctionParameter name (length params) t]) [] params
       in let ctx' = foldl (\ c n -> let (FunctionParameter name _ _) = n in Map.insert name n c) ctx params'
           in Function name params' ret_type (naming' ctx' body)
    Identifier name ->
      case Map.lookup name ctx of 
        Nothing -> ast
        Just f -> f
    Call callee args ->
      Call (naming' ctx callee) (naming' ctx <$> args)
    _ -> ast
