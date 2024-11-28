module Net.Redex
  ( Redex
  , RedexF(..)
  , flipRedex
  , sortRedex
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (get, put, runState)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Tuple (Tuple(..))
import Net.Tree (class Isomorphic, TreeF, VarLabel, isomorphic)

data RedexF a = Redex (TreeF a) (TreeF a)

derive instance Functor RedexF

instance Foldable RedexF where
  foldl f b0 (Redex l r) = foldl f (foldl f b0 l) r
  foldr f b0 (Redex l r) = foldr f (foldr f b0 r) l
  foldMap = foldMapDefaultL

type Redex = RedexF VarLabel

derive instance Eq a => Eq (RedexF a)

instance Show Redex where
  show (Redex x y) = show x <> " ~ " <> show y

instance Ord Redex where
  compare r1 r2 =
    let
      (Redex x1 y1) = sortRedex r1
      (Redex x2 y2) = sortRedex r2
    in
      case compare x1 x2 of
        EQ -> compare y1 y2
        x -> x

flipRedex :: forall a. (RedexF a) -> (RedexF a)
flipRedex (Redex x y) = (Redex y x)

sortRedex :: Redex -> Redex
sortRedex r@(Redex x y) = if x < y then r else flipRedex r

-- Given (A ~ B), (C ~ D), first check tree isomorphisms A <-> C & B <-> D
-- then try A <-> D and B <-> C
instance Ord a => Isomorphic RedexF a where
  isomorphic r1 r2 = do
    varMap <- get
    let Tuple isIso s = runState (liftIso r1 r2) varMap
    if isIso then do
      put s
      pure true
    else
      liftIso r1 (flipRedex r2)
    where
    liftIso (Redex x1 y1) (Redex x2 y2) = (lift2 (&&) (isomorphic x1 x2) (isomorphic y1 y2))