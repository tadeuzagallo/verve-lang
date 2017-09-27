{-# LANGUAGE FlexibleContexts #-}
module Typing.Ctx
  ( Ctx()
  , defaultCtx
  , addType
  , getType
  , addValueType
  , getValueType
  , addImplementation
  , getImplementations
  , addInterface
  , getInterface
  , addInstanceVars
  , getInstanceVars
  , tImportModule
  ) where

import Typing.State
import Typing.Substitution
import Typing.TypeError
import Typing.Types hiding (list)

import qualified Typing.Types as Types (list)

data Ctx = Ctx { types :: [(String, Type)]
               , values :: [(String, Type)]
               , implementations :: [(String, [(Type, [BoundVar])])]
               , interfaces :: [(String, Intf)]
               , instanceVars :: [(Type, [(String, Type)])]
               } deriving (Eq)

getType :: String -> Ctx -> Tc Type
getType n ctx =
  case lookup n (types ctx) of
    Nothing -> throwError (UnknownType n)
    Just t -> instantiate t

getValueType :: String -> Ctx -> Tc Type
getValueType n ctx =
  case lookup n (values ctx) of
    Nothing -> throwError (UnknownVariable n)
    Just t -> instantiate t

addType :: Ctx -> (String, Type) -> Ctx
addType ctx (n, ty) = ctx { types = (n, ty) : types ctx }

addValueType :: Ctx -> (String, Type) -> Ctx
addValueType ctx (n, ty) = ctx { values = (n, ty) : values ctx }

getImplementations :: String -> Ctx -> Tc [(Type, [BoundVar])]
getImplementations n ctx =
  case lookup n (implementations ctx) of
    Nothing -> return []
    Just insts -> return insts

addImplementation :: Ctx -> (String, (Type, [BoundVar])) -> Tc Ctx
addImplementation ctx (n, inst) = do
  insts <- getImplementations n ctx
  return $ ctx { implementations = update n (inst : insts) (implementations ctx) }
    where
      update key value [] = [(key, value)]
      update key value ((k,_):rest) | k == key = (key, value) : rest
      update key value (x:xs) = x : update key value xs

defaultCtx :: Ctx
defaultCtx =
  Ctx { types = [ ("Int", int)
                , ("Float", float)
                , ("Char", char)
                , ("String", string)
                , ("Void", void)
                , ("List", forall [T] $ list T)
                , ("Bool", bool)
                ]
      , values = [ ("string_print", [string] ~> void)
                 , ("int_add", [int, int] ~> int)
                 , ("int_sub", [int, int] ~> int)
                 , ("int_mul", [int, int] ~> int)
                 , ("int_div", [int, int] ~> int)
                 , ("True", bool)
                 , ("False", bool)
                 , ("Nil", forall [T] $ list T)
                 , ("Cons", forall [T] $ [var T, list T] ~> list T)
                 ]
      , implementations = []
      , interfaces = []
      , instanceVars = []
      }

-- HELPERS

list :: FakeVar -> Type
list ty = Types.list (var ty)

forall :: [FakeVar] -> Type -> Type
forall vs (Fun [] params args) =
  let vs' = map (flip (,) [] . tyvar) vs
   in Fun vs' params args

forall vs ty =
  TyAbs (map tyvar vs) ty

var :: FakeVar -> Type
var name = Var (tyvar name) []

getInterface :: String -> Ctx -> Tc Intf
getInterface n ctx =
  case lookup n (interfaces ctx) of
    Nothing -> throwError (UnknownInterface n)
    Just t -> return t

addInterface :: Ctx -> (String, Intf) -> Ctx
addInterface ctx (n, ty) = ctx { interfaces = (n, ty) : interfaces ctx }

addInstanceVars :: Ctx -> (Type, [(String, Type)]) -> Ctx
addInstanceVars ctx (cls, vars) = ctx { instanceVars = (cls, vars) : instanceVars ctx }

getInstanceVars :: Type -> Ctx -> Tc [(String, Type)]
getInstanceVars cls@(Cls name) ctx =
  case lookup cls (instanceVars ctx) of
    Nothing -> throwError $ UnknownType name
    Just t -> return t

getInstanceVars _ _ = undefined


-- MODULE IMPORTATION
tImportModule :: [String] -> Ctx -> Ctx -> Ctx
tImportModule items prevCtx impCtx =
  prevCtx { values = filterImports (values impCtx) ++ values prevCtx
          , types = filterImports (types impCtx) ++ types prevCtx
          , implementations = implementations impCtx
          }

 where
   filterImports = filter $ flip elem items . fst
