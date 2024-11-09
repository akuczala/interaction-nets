module Utils
  ( SymmMap
  , fixedPoint
  , symmInsert
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as M

fixedPoint :: forall a. Eq a => (a -> a) -> (a -> a)
fixedPoint f = fixed
  where
  fixed a = if nextA == a then a else fixed nextA
    where
    nextA = f a

type SymmMap k = Map k k

symmInsert :: forall k. Ord k => k -> k -> SymmMap k -> SymmMap k
symmInsert k1 k2 = M.insert k1 k2 >>> M.insert k2 k1