module Random
  ( RANDOM
  , RandomF(..)
  , _random
  , randomBool
  , randomCombination
  , randomInt
  , randomPairing
  , randomPermutation
  , randomRedex
  , randomTree
  , randomUniform
  , randomlyRenamed
  , runRandom
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (State, gets, modify_)
import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Effect.Random as R
import Nets (class HasVars, Redex(..), Tree(..), VarGenState, getVars, makeDelta, makeGamma, mapVars, newVar, rename)
import Random.LCG (Seed, lcgNext, unSeed)
import Run (Run, EFFECT, interpret, liftEffect, on, send)
import Run as Run
import Run.State (STATE)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Utils (liftState)

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

randomPermutation :: forall r a. Array a -> Run (RANDOM + r) (Array a)
randomPermutation arr = randomCombination (A.length arr) arr

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

-- runPseudoRandom :: forall r. Run (RANDOM + r) ~> Run (EFFECT + r)
-- runRandom = interpret (on _random handleRandom send)

randomTree :: forall r. Int -> Run (STATE VarGenState + RANDOM + r) Tree
randomTree i = do
  rand <- randomInt 0 i
  case rand of
    0 -> do
      rv <- randomUniform
      if rv < 0.2 then pure Epsilon else map Var (liftState newVar)
    _ -> do
      rb <- randomBool
      let op = if rb then makeGamma else makeDelta
      lift2 op (randomTree $ i - 1) (randomTree $ i - 1)

randomRedex :: forall r. Int -> Run (STATE VarGenState + RANDOM + r) Redex
randomRedex i = do
  redex <- lift2 Redex (randomTree i) (randomTree i)
  vars <- liftState $ gets (A.fromFoldable <<< _.vars)
  nConnections <- randomInt 0 (div (A.length vars) 2)
  connections <- randomPairing nConnections vars
  pure $ mapVars (\v -> Var $ fromMaybe v $ M.lookup v connections) redex

randomlyRenamed :: forall r a. HasVars a => a -> Run (RANDOM + r) a
randomlyRenamed r = do
  let vars = A.fromFoldable $ getVars r
  shuffledVars <- randomPermutation vars
  let varMap = M.fromFoldable $ A.zip vars shuffledVars
  pure $ rename varMap r