module Test.Utils
  ( testRandomCombination
  , testRandom
  )
  where

import Prelude

import Data.Array as A
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Console (logShow)
import Nets (Tree)
import Random (randomCombination, randomPairing, runRandom)
import Run (runBaseEffect)
import Test.QuickCheck (class Arbitrary, arbitrary, assertEquals, quickCheck)

testRandom :: Effect Unit
testRandom = testRandomCombination

testRandomCombination :: Effect Unit
testRandomCombination = do
  combo <- runBaseEffect <<< runRandom $ randomCombination 5 (A.range 0 11)
  logShow combo
  pure $ A.length combo == 5 && A.length (A.nub combo) == 5
  >>= quickCheck

-- testRandomPairing :: Effect Unit
-- testRandomPairing = do
--   pairing <- randomPairing 1 
