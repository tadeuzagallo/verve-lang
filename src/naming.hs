module Naming (naming) where

import AST

import qualified Data.Map as Map

type Context = (Map.Map String AST)

naming :: AST -> AST
naming = naming' Map.empty

naming' :: Context -> AST -> AST
naming' ctx ast =
  case ast of
    program@Program { expressions=nodes } ->
      program { expressions=naming' ctx <$> nodes }

    block@Block { nodes=nodes } ->
      block { nodes=naming' ctx <$> nodes }

    list@List { items=items } ->
      list { items=naming' ctx <$> items }

    unop@UnaryOp { operand=operand } ->
      unop { operand=(naming' ctx operand) }

    binop@BinaryOp { lhs=lhs, rhs=rhs } ->
      binop { lhs=(naming' ctx lhs), rhs=(naming' ctx rhs) }

    iff@If { condition=cond, consequent=conseq, alternate=alt } ->
      iff { condition=(naming' ctx cond), consequent=(naming' ctx conseq), alternate=(naming' ctx <$> alt) }

    fn@Function { params=params, body=body } ->
      let params' = foldl (\ params p -> params ++ [p { index=length params }]) [] params in
      let ctx' = foldl (\ c n -> Map.insert (name n) n c) ctx params' in
      fn { params=params', body=(naming' ctx' body) }

    id@Identifier { name=name } ->
      case Map.lookup name ctx of 
        Nothing -> id
        Just f -> f

    call@Call { callee=callee, arguments=args } ->
      call { callee=(naming' ctx callee), arguments=(naming' ctx <$> args) }

    _ -> ast
