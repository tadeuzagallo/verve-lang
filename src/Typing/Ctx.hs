{-# LANGUAGE FlexibleContexts #-}
module Typing.Ctx
  ( Ctx()
  , defaultCtx
  , addType
  , getType
  , addValueType
  , getValueType
  , addInstance
  , getInstances
  , tImportModule
  , getModule
  ) where

import Absyn.Meta
import Typing.State
import Typing.Substitution
import Typing.TypeError
import Typing.Types hiding (list)

import qualified Typing.Types as Types (list)

import Data.Bifunctor (first, second)
import Data.List ((\\), union)

data Ctx = Ctx { types :: [(String, Type)]
               , values :: [(String, Type)]
               , instances :: [(String, [(Type, [(Var, [Type])])])]
               , modules :: [(String, Ctx)]
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

getInstances :: String -> Ctx -> Tc [(Type, [(Var, [Type])])]
getInstances n ctx =
  case lookup n (instances ctx) of
    Nothing -> return []
    Just insts -> return insts

addInstance :: Ctx -> (String, (Type, [(Var, [Type])])) -> Tc Ctx
addInstance ctx (n, inst) = do
  insts <- getInstances n ctx
  return $ ctx { instances = update n (inst : insts) (instances ctx) }
    where
      update key value [] = [(key, value)]
      update key value ((k,_):rest) | k == key = (key, value) : rest
      update key value (x:xs) = x : update key value xs

emptyCtx :: Ctx
emptyCtx = Ctx [] [] [] []

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
      , values = [ ("int_print", [int] ~> void)
                 , ("int_add", [int, int] ~> int)
                 , ("int_sub", [int, int] ~> int)
                 , ("int_mul", [int, int] ~> int)
                 , ("int_div", [int, int] ~> int)
                 , ("True", bool)
                 , ("False", bool)
                 , ("Nil", forall [T] $ list T)
                 , ("Cons", forall [T] $ [var T, list T] ~> list T)
                 ]
      , instances = []
      , modules = []
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

-- MODULE IMPORTATION

getModule :: String -> Ctx -> Tc Ctx
getModule n ctx =
  case lookup n (modules ctx) of
    Nothing -> throwError (UnknownModule n)
    Just ctx -> return ctx

tImportModule :: Import -> Ctx -> Ctx -> Ctx
tImportModule (Import isGlobal mod alias items) prevCtx impCtx =
  finalCtx
  where
    finalCtx =
      if isGlobal
         then addCtx prevCtx filteredCtx
         else prevCtx { modules = addModule name (modules prevCtx)
                      , instances = instances impCtx
                      }

    addModule [] _ = undefined
    addModule [name] mods =
      case lookup name mods of
        Nothing -> (name, filteredCtx) : mods
        _ -> undefined

    addModule (n:ns) mods =
      let aux [] =
            [(n, emptyCtx { modules = (addModule ns []) })]
          aux ((m, ctx) : ms) | m == n =
            (m, ctx { modules = (addModule ns $ modules ctx) }) : ms
          aux (m:ms) =
            m : aux ms
         in aux mods

    name = maybe mod (:[]) alias

    addCtx c1 c2 =
      c1 { types = types c1 `union` types c2
         , values = values c1 `union` values c2
         , instances = instances impCtx
         }

    filteredCtx =
      case items of
        Nothing -> newCtx
        Just i -> filterItems newCtx (mkFilter i)

    filterItems ctx (vs, ts) =
      ctx { types =  filter (flip elem ts . fst) (types ctx)
          , values = filter (flip elem vs . fst) (values ctx)
          }

    mkFilter [] = ([], [])
    mkFilter (ImportValue v : is) =
      first (v:) $ mkFilter is
    mkFilter (ImportType t cs : is) =
      second (t:) $ mkFilter (map ImportValue cs ++ is)

    newCtx = Ctx { types = (types impCtx) \\ (types defaultCtx)
                 , values = (values impCtx) \\ (values defaultCtx)
                 , instances = []
                 , modules = []
                 }
