module Test.Random
  ( handleRandomGen
  , testRandom
  , testRandomCombination
  )
  where

import Prelude

import Control.Monad.Gen.Class (chooseBool)
import Data.Array as A
import Effect (Effect)
import Random (RandomF(..), _random, randomCombination, randomPermutation)
import Run (case_, interpret, on)
import Test.QuickCheck (class Arbitrary, quickCheck, quickCheckGen)
import Test.QuickCheck.Gen (Gen, choose, chooseInt, repeatable, suchThat, vectorOf)

handleRandomGen :: RandomF ~> Gen
handleRandomGen = case _ of
    RandomUniform next -> map next $ choose 0.0 1.0
    RandomInt low high next -> map next $ chooseInt low high
    RandomBool next -> map next $ chooseBool

newtype ShuffleArray a = ShuffleArray (Array a -> Array a)

-- dogen :: forall r a. Run (RANDOM + r) a -> Gen a
-- dogen = interpret (case_ # on _random handleRandomGen)

instance Arbitrary a => Arbitrary (ShuffleArray a) where
  arbitrary = map ShuffleArray $ repeatable $ \t ->
    randomPermutation t # interpret (case_ # on _random handleRandomGen)

testRandom :: Effect Unit
testRandom = do
  testRandomCombination
  testRandomPermutation

testRandomCombination :: Effect Unit
testRandomCombination = quickCheckGen do
  -- generate arrays with unique elements
  arr <- suchThat (vectorOf 10 (chooseInt 0 500)) $ \a -> A.length (A.nub a) == A.length a
  let n = 5
  combo <- randomCombination n arr # interpret (case_ # on _random handleRandomGen)
  pure $ A.length combo == n && A.length (A.nub combo) == n

testRandomPermutation :: Effect Unit
testRandomPermutation = quickCheck $
  \(x :: Array Int) (ShuffleArray f :: ShuffleArray Int) ->
    let x' = f x in
    A.sort x == A.sort x' && A.length x == A.length x'

-- testRandomPairing :: Effect Unit
-- testRandomPairing = do
--   pairing <- randomPairing 1 
