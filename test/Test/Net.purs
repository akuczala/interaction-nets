module Test.Net
  ( testNet
  )
  where

import Prelude

import Control.Monad.Gen (chooseBool)
import Control.Monad.State (runState, runStateT)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (logShow)
import Nets (Redex(..), Tree(..), VarLabel, initReduceState, isomorphic, makeDelta, makeGamma, reduceArr, substitute)
import Random (RandomF(..), _random, randomRedex, randomTree, runRandom)
import Run (case_, interpret, on)
import Run as Run
import Run.State as Run.State
import Test.QuickCheck (class Arbitrary, quickCheck)
import Test.QuickCheck.Gen (Gen, choose, chooseInt)
import Utils (fixedPoint)

testNet :: Effect Unit
testNet = do
    testReduce
    testIsomorphism

-- test that random trees and redexes are isomorphic to themselves
-- TODO: test isomorphism on renamed variables
testIsomorphism :: Effect Unit
testIsomorphism = do
    quickCheck $ \(RandomTree t) -> fst $ unwrap $ runStateT (isomorphic t t) M.empty
    quickCheck $ \(RandomRedex r) -> fst $ unwrap $ runStateT (isomorphic r r) M.empty

testReduce :: Effect Unit
testReduce = do
  let reduced = fixedPoint (reduceArr >>> substitute) [redex2]
  logShow $ reduced
  logShow $ runState (
    isomorphic (
      simpleTree {a: "a", b: "b", c: "c", d: "b"}
      ) (
        simpleTree {a: "a", b: "d", c: "e", d: "d"}
        )
    ) M.empty
  Tuple t _ <- Run.runBaseEffect $ Run.State.runState initReduceState (runRandom $ randomTree 5)
  logShow t

simpleRedex :: Redex
simpleRedex = (
  Redex (makeGamma (Var "a") (Var "b")) (makeDelta (Var "c") (Var "d"))
  )

simpleTree
  :: {a :: VarLabel, b :: VarLabel, c :: VarLabel, d :: VarLabel}
  -> Tree
simpleTree vs = (
  makeDelta (makeGamma (Var vs.a) (Var vs.b)) (makeDelta (Var vs.c) (Var vs.d))
  )

simpleTree2 :: Tree
simpleTree2 = makeGamma (Var "c") (Var "d")

lambdaId :: VarLabel -> Tree
lambdaId s = makeGamma (Var s) (Var s)

lambdaApplySelf :: Tree
lambdaApplySelf = makeGamma (makeDelta (Var "x1") (makeGamma (Var "x1") (Var "bod"))) (Var "bod")

redex2 :: Redex
redex2 = Redex lambdaApplySelf (makeGamma (lambdaId "a") (Var "root"))

handleRandomGen :: RandomF ~> Gen
handleRandomGen = case _ of
    RandomUniform next -> map next $ choose 0.0 1.0
    RandomInt low high next -> map next $ chooseInt low high
    RandomBool next -> map next $ chooseBool

newtype RandomTree = RandomTree Tree
newtype RandomRedex = RandomRedex Redex

instance Arbitrary RandomTree where
    arbitrary = map RandomTree $ thing # interpret (case_ # on _random handleRandomGen)
        where
        thing = map snd $ Run.State.runState initReduceState (randomTree 3)

instance Arbitrary RandomRedex where
    arbitrary = map RandomRedex $ thing # interpret (case_ # on _random handleRandomGen)
        where
        thing = map snd $ Run.State.runState initReduceState (randomRedex 3)

-- runRandom :: forall r. Run (EFFECT + RANDOM + r) ~> Run (EFFECT + r)
-- runRandom = interpret (on _random handleRandom send)