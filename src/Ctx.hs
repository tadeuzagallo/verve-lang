{-# LANGUAGE FlexibleContexts #-}
module Ctx
  ( Ctx()
  , defaultCtx
  , addType
  , getType
  , addValueType
  , getValueType
  ) where

import TypeError
import Types

import Control.Monad.Except (MonadError, throwError)

data Ctx = Ctx { types :: [(Var, Type)]
               , values :: [(String, Type)]
               }

getType :: MonadError TypeError m => Var -> Ctx -> m Type
getType n ctx =
  case lookup n (types ctx) of
    Nothing -> throwError (UnknownType $ show n)
    Just t -> return t

getValueType :: MonadError TypeError m => String -> Ctx -> m Type
getValueType n ctx =
  case lookup n (values ctx) of
    Nothing -> throwError (UnknownVariable n)
    Just t -> return t

addType :: Ctx -> (Var, Type) -> Ctx
addType ctx (n, ty) = ctx { types = (n, ty) : types ctx }

addValueType :: Ctx -> (String, Type) -> Ctx
addValueType ctx (n, ty) = ctx { values = (n, ty) : values ctx }

defaultCtx :: Ctx
defaultCtx =
  Ctx { types = [ (var "Int", int)
                , (var "Float", float)
                , (var "Char", char)
                , (var "String", string)
                , (var "Void", void)
                , (var "List", genericList)
                , (var "Bool", bool)
                ]
      , values = [ ("int_print", [int] ~> void)
                 , ("int_add", [int, int] ~> int)
                 , ("int_sub", [int, int] ~> int)
                 , ("int_mul", [int, int] ~> int)
                 , ("True", bool)
                 , ("False", bool)
                 , ("Nil", genericList)
                 , ("Cons", Fun [(var "T", [Top])] [Var (var "T") [Top], list $ Var (var "T") [Top]] (list $ Var (var "T") [Top]))
                 ]
      }

