module Test.Net
  ( 
  testNet
  )
  where

import Prelude

import Control.Monad.State (evalState, runState, runStateT)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (logShow)
import Nets (Redex(..), Tree(..), VarLabel, initReduceState, isomorphic, makeDelta, makeGamma, reduceArr, substitute)
import Random (_random, randomRedex, randomTree, randomlyRenamedTree, runRandom)
import Run (case_, interpret, on)
import Run as Run
import Run.State as Run.State
import Test.QuickCheck (class Arbitrary, assertEquals, assertNotEquals, quickCheck, (<?>))
import Test.QuickCheck.Gen (repeatable)
import Test.Random (handleRandomGen)
import Utils (emptyBimap, fixedPoint)

testNet :: Effect Unit
testNet = do
    testReduce
    testIsomorphismCase
    testIsomorphism

-- test that random trees and redexes are isomorphic to themselves
-- TODO: test isomorphism on renamed variables

testIsomorphismCase :: Effect Unit
testIsomorphismCase = do
  let b = Var "b"
  let c = Var "c"
  let d = Var "d"
  let t1 = (makeGamma (makeGamma (makeGamma b c) Epsilon) d)
  let t2 = (makeGamma (makeGamma (makeGamma d b) Epsilon) c)
  quickCheck $ assertEquals true $ evalState (isomorphic t1 t2) emptyBimap
  let t3 = (makeGamma (makeGamma (makeGamma d b) Epsilon) b)
  quickCheck $ assertNotEquals true $ evalState (isomorphic t1 t3) emptyBimap
testIsomorphism :: Effect Unit
testIsomorphism = do
    let msg t t' = show t <> " and " <> show t' <> " not isomorphic"
    quickCheck $ \(RandomTree t) (RandomlyRenameTree rename) ->
      let
        t' = rename t
      in
        (fst $ unwrap $ runStateT (isomorphic t t') emptyBimap) <?> msg t t'
    quickCheck $ \(RandomRedex r) ->
      fst $ unwrap $ runStateT (isomorphic r r) emptyBimap

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
    ) emptyBimap
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
lambdaApplySelf = makeGamma
  (makeDelta (Var "x1") (makeGamma (Var "x1") (Var "bod")))
  (Var "bod")

redex2 :: Redex
redex2 = Redex lambdaApplySelf (makeGamma (lambdaId "a") (Var "root"))

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

newtype RandomlyRenameTree = RandomlyRenameTree (Tree -> Tree)
instance Arbitrary RandomlyRenameTree where
  arbitrary = map RandomlyRenameTree $ repeatable $ \t ->
    randomlyRenamedTree t # interpret (case_ # on _random handleRandomGen)

-- runRandom :: forall r. Run (EFFECT + RANDOM + r) ~> Run (EFFECT + r)
-- runRandom = interpret (on _random handleRandom send)
