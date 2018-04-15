module Util.Env
  ( Env(..)
  , Field
  , deleteBetweenField
  , insert
  , Util.Env.lookup
  ) where

type Field key value = [(key, value)]

class (Show t, Show (KeyType t), Show (ValueType t), Show (InterfaceType t)) => Env t where
  type KeyType t :: *
  type ValueType t :: *
  type InterfaceType t :: *

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
  deleteBetween :: t -> t -> t -> t

deleteBetweenField :: Field a b -> Field a b -> Field a b -> Field a b
deleteBetweenField from to current =
  take (length current - length to) current ++ from

lookup :: (Eq a, Env t) => (t -> Field a b) -> a -> t -> Maybe b
lookup proj key env =
  Prelude.lookup key (proj env)

insert :: (Eq a, Env t) => (t -> Field a b) -> (t -> Field a b -> t) -> a -> b -> t -> t
insert proj inj key value env =
  inj env ((key, value) : proj env)
