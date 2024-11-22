module Test.Net
  ( 
  testNet
  )
  where

import Prelude

import Control.Monad.State (evalState)
import Effect (Effect)
import Nets (class HasVars, Redex(..), Tree(..), VarLabel, evalIso, flipRedex, initVarGenState, isomorphic, makeDelta, makeGamma, reduceArr, substitute)
import Random (_random, randomRedex, randomTree, randomlyRenamed)
import Run (case_, interpret, on)
import Run.State as Run.State
import Test.QuickCheck (class Arbitrary, assertEquals, assertNotEquals, quickCheck, quickCheck', (<?>))
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

msg :: forall a. Show a => a -> a -> String
msg a b = show a <> " and " <> show b <> " not isomorphic"
testIsomorphism :: Effect Unit
testIsomorphism = do
    quickCheck $ \(RandomTree t) (RandomlyRename rename) ->
      let
        t' = rename t
      in
        evalIso t t' <?> msg t t'
    quickCheck $ \(RandomRedex r) -> evalIso r r
    quickCheck $ \(RandomRedex r) (RandomlyRename rename) ->
      let
        r' = rename r
      in
        evalIso r r' <?> msg r r'
    quickCheck $ \(RandomRedex r) -> evalIso r (flipRedex r)

testReduce :: Effect Unit
testReduce = do
  let reduced = fixedPoint (reduceArr >>> substitute) [redex2]
  quickCheck' 1 $ evalIso reduced [Redex (Var "theroot") (lambdaId "joined")]

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
    thing = Run.State.evalState initVarGenState (randomTree 3)

instance Arbitrary RandomRedex where
  arbitrary = map RandomRedex $ thing # interpret (case_ # on _random handleRandomGen)
    where
    thing = Run.State.evalState initVarGenState (randomRedex 3)

newtype RandomlyRename a = RandomlyRename (a -> a)
instance HasVars a => Arbitrary (RandomlyRename a) where
  arbitrary = map RandomlyRename $ repeatable $ \t ->
    randomlyRenamed t # interpret (case_ # on _random handleRandomGen)

-- runRandom :: forall r. Run (EFFECT + RANDOM + r) ~> Run (EFFECT + r)
-- runRandom = interpret (on _random handleRandom send)
