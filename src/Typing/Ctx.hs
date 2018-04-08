module Typing.Ctx
  ( Ctx
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
  , deleteBetween
  ) where

import Lib.Registry
import Typing.State
import Typing.Substitution
import Typing.TypeError
import Typing.Types

import Util.Error

data Ctx = Ctx { types :: [(String, Type)]
               , values :: [(String, Type)]

               , interfaces :: [(String, Intf)]


               , implementations :: [(String, [(Type, [BoundVar])])]
               , instanceVars :: [(Type, [(String, Type)])]
               } deriving (Eq)

defaultCtx :: Ctx
defaultCtx =
  Ctx { types = decl <$> filter isType registry
      , values = decl <$> filter (\x -> isValue x || isCtor x) registry
      , implementations = []
      , interfaces = []
      , instanceVars = []
      }

getType :: String -> Tc Type
getType n = do
  ctx <- getCtx
  case lookup n (types ctx) of
    Nothing -> throwError (UnknownType n)
    Just t -> instantiate t

getValueType :: String -> Tc Type
getValueType n = do
  ctx <- getCtx
  case lookup n (values ctx) of
    Nothing -> throwError (UnknownVariable n)
    Just t -> instantiate t

addType :: (String, Type) -> Tc ()
addType (n, ty) =
  modifyCtx $ \ctx -> ctx { types = (n, ty) : types ctx }

addValueType :: (String, Type) -> Tc ()
addValueType (n, ty) =
  modifyCtx $ \ctx -> ctx { values = (n, ty) : values ctx }

getImplementations :: String -> Tc [(Type, [BoundVar])]
getImplementations n = do
  ctx <- getCtx
  case lookup n (implementations ctx) of
    Nothing -> return []
    Just insts -> return insts

addImplementation :: (String, (Type, [BoundVar])) -> Tc ()
addImplementation (n, inst) = do
  insts <- getImplementations n
  modifyCtx $ \ctx -> ctx { implementations = update n (inst : insts) (implementations ctx) }
    where
      update key value [] = [(key, value)]
      update key value ((k,_):rest) | k == key = (key, value) : rest
      update key value (x:xs) = x : update key value xs

getInterface :: String -> Tc Intf
getInterface n = do
  ctx <- getCtx
  case lookup n (interfaces ctx) of
    Nothing -> throwError (UnknownInterface n)
    Just t -> return t

addInterface :: (String, Intf) -> Tc ()
addInterface (n, ty) =
  modifyCtx $ \ctx -> ctx { interfaces = (n, ty) : interfaces ctx }

addInstanceVars :: (Type, [(String, Type)]) -> Tc ()
addInstanceVars (cls, vars) =
  modifyCtx $ \ctx -> ctx { instanceVars = (cls, vars) : instanceVars ctx }

getInstanceVars :: Type -> Tc [(String, Type)]
getInstanceVars cls@(Cls name) = do
  ctx <- getCtx
  case lookup cls (instanceVars ctx) of
    Nothing -> throwError $ UnknownType name
    Just t -> return t

getInstanceVars _ = undefined


-- MODULE IMPORTATION
tImportModule :: [String] -> Ctx -> Ctx -> Ctx
tImportModule items prevCtx impCtx =
  prevCtx { values = filterImports (values impCtx) ++ values prevCtx
          , types = filterImports (types impCtx) ++ types prevCtx
          , implementations = implementations impCtx
          , instanceVars = instanceVars impCtx ++ instanceVars prevCtx
          }

 where
   filterImports = filter $ flip elem items . fst

-- This is a confusing signature, the idea is that
--  that everything that got added to the `current`
--  context in between `from` and `to` should be
--  removed. Everything added after `to` should be
--  preserved though. e.g.:
--
--                  current
--  +------------------------------------+
--  [ x: S, y: T, z: U, a: A, b: B, x: T ]
--  +-----------------------+
--  +-----------+     to
--     from
--
--  `delete from to current` would result in
--
--                 current - to
--                +------------+
--    [ x: S, y: T, b: B, x: T ]
--    +-----------+
--        from
--
--  NOTE: the implementation is mirrored, since items get
--   added to the front of the list instead of the end
--
deleteBetween :: Ctx -> Ctx -> Ctx -> Ctx
deleteBetween from to current =
  current { types  = take (length (types  current) - length (types  to)) (types  current) ++ types  from
          , values = take (length (values current) - length (values to)) (values current) ++ values from
          }
