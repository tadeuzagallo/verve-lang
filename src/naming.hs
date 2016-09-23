module Naming (naming) where

import AST

import qualified Data.Map as Map

import Control.Monad (liftM)
import Control.Monad.State (State, evalState, gets, modify)

type Context = (Map.Map String AST)

naming :: AST -> AST
naming ast = evalState (naming' ast) Map.empty

naming' :: AST -> State Context AST
naming' program@Program { expressions=nodes } = do
  nodes' <- mapM naming' nodes
  return program { expressions=nodes' }

naming' block@Block { nodes=nodes } = do
  nodes' <- mapM naming' nodes
  return block { nodes=nodes' }

naming' list@List { items=items } = do
  items' <- mapM naming' items
  return list { items=items' }

naming' unop@UnaryOp { operand=operand } = do
  operand' <- naming' operand
  return unop { operand=operand' }

naming' binop@BinaryOp { lhs=lhs, rhs=rhs } = do
  lhs' <- naming' lhs
  rhs' <- naming' rhs
  return binop { lhs=lhs', rhs=rhs' }

naming' iff@If { condition=cond, consequent=conseq, alternate=alt } = do
  cond' <- naming' cond
  conseq' <- naming' conseq
  {- TODO: there must be a cleaner way of writing this ⬇️ -}
  alt' <- case alt of
            Nothing -> return Nothing
            Just a -> naming' a >>= \a' -> return $ Just a'
  return iff { condition=cond', consequent=conseq', alternate=alt' }

naming' fn@Function { params=params, body=body } = do
  let params' = foldl (\ params p -> params ++ [p { index=length params }]) [] params
  mapM_ (\n -> modify $ Map.insert (name n) n) params'
  body' <- naming' body
  return fn { params=params', body=body' }

naming' id@Identifier { pos=pos, name=name } = do
  value <- gets $ Map.lookup name
  return $ case value of
    Nothing -> id
    Just f -> f { pos=pos }

naming' call@Call { callee=callee, arguments=args } = do
  callee' <- naming' callee
  args' <- mapM naming' args
  return call { callee=callee', arguments=args' }

naming' intf@Interface { name=name, functions=fns } = do
  fns' <- mapM naming' fns
  return intf { functions=fns' }

naming' impl@Implementation { name=name, impl_type=t, functions=fns } = do
  fns' <- mapM naming' fns
  return impl { functions=fns' }

naming' ast = return ast
