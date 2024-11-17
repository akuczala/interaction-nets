module Random
  ( RANDOM
  , RandomF(..)
  , _random
  , randomBool
  , randomCombination
  , randomInt
  , randomPairing
  , randomUniform
  , runRandom
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (State, gets, modify_)
import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Random as R
import Random.LCG (Seed, lcgNext, unSeed)
import Run (Run, VariantF, EFFECT, case_, interpret, liftEffect, on, send)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data RandomF a
  = RandomUniform (Number -> a)
  | RandomInt Int Int (Int -> a)
  | RandomBool (Boolean -> a)

derive instance Functor RandomF

type RANDOM r = (random :: RandomF | r)

_random = Proxy :: Proxy "random"

randomUniform :: forall r. Run (RANDOM + r) Number
randomUniform = Run.lift _random (RandomUniform identity)

randomInt :: forall r. Int -> Int -> Run (RANDOM + r) Int
randomInt low high = Run.lift _random (RandomInt low high identity)

randomBool :: forall r. Run (RANDOM + r) Boolean
randomBool = Run.lift _random (RandomBool identity)

randomCombination :: forall r a. Int -> Array a -> Run (RANDOM + r) (Array a)
randomCombination 0 _ = pure []
randomCombination n arr = do
  randIndex <- randomInt 0 (A.length arr - 1)
  let item = fromMaybe [] $ map A.singleton $ A.index arr randIndex
  let remaining = fromMaybe arr $ A.deleteAt randIndex arr
  lift2 (<>) (pure item) (randomCombination (n - 1) remaining)

randomPairing :: forall r a. Ord a => Int -> Array a -> Run (RANDOM + r) (Map a a)
randomPairing nPairs vars = do
  --nConnections <- randomInt 0 (div (A.length vars) 2)
  connectVars <- randomCombination (nPairs * 2) vars
  pure $ M.fromFoldable $ A.zip connectVars (A.drop nPairs connectVars)

handleRandom :: forall r. RandomF ~> Run (EFFECT + r)
handleRandom = case _ of
  RandomUniform next -> liftEffect $ map next $ R.random
  RandomInt low high next -> liftEffect $ map next $ R.randomInt low high
  RandomBool next -> liftEffect $ map next $ R.randomBool

handlePseudoRandom :: RandomF ~> State Seed
handlePseudoRandom rf = do
  i <- gets unSeed
  modify_ lcgNext
  pure $ case rf of
    RandomUniform next -> next $ 0.0 -- UNIMPLEMENTED
    RandomInt low high next -> next $ mod i (high - low) + low
    RandomBool next -> next $ mod i 2 == 1

runRandom :: forall r. Run (EFFECT + RANDOM + r) ~> Run (EFFECT + r)
runRandom = interpret (on _random handleRandom send)

