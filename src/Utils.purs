module Utils
  ( Bimap
  , SymmMap
  , bimapInsert
  , emptyBimap
  , fixedPoint
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

fixedPoint :: forall a. Eq a => (a -> a) -> (a -> a)
fixedPoint f = fixed
  where
  fixed a = if nextA == a then a else fixed nextA
    where
    nextA = f a

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