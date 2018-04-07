module Util.Env
  ( Env(..)
  , Field
  , deleteBetweenField
  , insert
  , Util.Env.lookup
  ) where

type Field key value = [(key, value)]

deleteBetweenField :: Field a b -> Field a b -> Field a b -> Field a b
deleteBetweenField from to current =
  take (length current - length to) current ++ from

class Env t where
  type KeyType t
  type ValueType t
  type InterfaceType t

  deleteBetween :: t -> t -> t -> t

lookup :: (Eq a, Env t) => (t -> Field a b) -> a -> t -> Maybe b
lookup proj key env =
  Prelude.lookup key (proj env)

insert :: (Eq a, Env t) => (t -> Field a b) -> (t -> Field a b -> t) -> a -> b -> t -> t
insert proj inj key value env =
  inj env ((key, value) : proj env)
