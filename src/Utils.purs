module Utils
  ( Bimap
  , SymmMap
  , bimapInsert
  , emptyBimap
  , fixedPoint
  , fixedPointOn
  , fixedPointPred
  , liftState
  , symmInsert
  ) where

import Prelude

import Control.Monad.State (State, StateT(..))
import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Run (Run)
import Run.State (STATE)
import Run.State as Run.State
import Type.Row (type (+))

fixedPointPred :: forall a. (a -> a -> Boolean) -> (a -> a) -> (a -> a)
fixedPointPred p f = fixed
  where
  fixed a = if p a nextA then a else fixed nextA
    where
    nextA = f a

fixedPointOn :: forall a b. Ord b => (a -> b) -> (a -> a) -> (a -> a)
fixedPointOn g = fixedPointPred (\a a' -> g a == g a')

fixedPoint :: forall a. Eq a => (a -> a) -> (a -> a)
fixedPoint = fixedPointPred (==)

type SymmMap k = Map k k

symmInsert :: forall k. Ord k => k -> k -> SymmMap k -> SymmMap k
symmInsert k1 k2 = M.insert k1 k2 >>> M.insert k2 k1

type Bimap k = Tuple (Map k k) (Map k k)

emptyBimap :: forall k. Bimap k
emptyBimap = Tuple M.empty M.empty

bimapInsert :: forall k. Ord k => k -> k -> Bimap k -> Bimap k
bimapInsert k1 k2 = bimap (M.insert k1 k2) (M.insert k2 k1)

-- for converting from a MTL state -> Run state
-- TODO: generalize to MonadState typeclass?
convertState :: forall s a. State s a -> Run.State.State s a
convertState (StateT f) = Run.State.State
  ( snd <<< unwrap <<< f
  )
  ( fst <<< unwrap <<< f
  )

liftState :: forall s a r. State s a -> Run ((STATE s) + r) a
liftState = convertState >>> Run.State.liftState