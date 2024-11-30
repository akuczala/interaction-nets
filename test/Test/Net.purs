module Test.Net
  ( RandomRedex(..)
  , RandomTree(..)
  , assign
  , testNet
  )
  where

import Prelude

import Control.Monad.State (evalState)
import Control.Plus (empty)
import Data.Array (range, (..))
import Data.Either (isRight)
import Data.Foldable (class Foldable, for_)
import Data.Lens (over, view)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Net (Net, NetF(..), Operator(..), Redex, RedexF(..), Tree, TreeF(..), VarLabel, VarGenState, _redexes, _root, evalIso, evalTree, flipRedex, initVarGenState, isomorphic, makeDelta, makeEpsilon, makeGamma, makeNumber, makeOperator, reduceNet, substitute, validateNetVars)
import Net.Tree (makeNumber, validateVars)
import Random (_random, randomRedex, randomTree, randomlyRenamed, runRandom)
import Run (case_, interpret, on, runBaseEffect)
import Run.State as Run.State
import Test.QuickCheck (class Arbitrary, assertEquals, assertNotEquals, quickCheck, quickCheck', (<?>))
import Test.QuickCheck.Gen (repeatable)
import Test.Random (handleRandomGen)
import Utils (emptyBimap, fixedPoint)

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
instance (Ord a, Functor t, Foldable t) => Arbitrary (RandomlyRename (t a)) where
  arbitrary = map RandomlyRename $ repeatable $ \t ->
    randomlyRenamed t # interpret (case_ # on _random handleRandomGen)

-- runRandom :: forall r. Run (EFFECT + RANDOM + r) ~> Run (EFFECT + r)
-- runRandom = interpret (on _random handleRandom send)

testNet :: Effect Unit
testNet = do
    testReduce
    testIsomorphismCase
    testIsomorphism
    testValidation
    testMath

testIsomorphismCase :: Effect Unit
testIsomorphismCase = do
  let b = Var "b"
  let c = Var "c"
  let d = Var "d"
  let t1 = (makeGamma (makeGamma (makeGamma b c) makeEpsilon) d)
  let t2 = (makeGamma (makeGamma (makeGamma d b) makeEpsilon) c)
  quickCheck $ assertEquals true $ evalState (isomorphic t1 t2) emptyBimap
  let t3 = (makeGamma (makeGamma (makeGamma d b) makeEpsilon) b)
  quickCheck $ assertNotEquals true $ evalState (isomorphic t1 t3) emptyBimap

testValidation :: Effect Unit
testValidation = do
  quickCheck $ \(RandomTree t) -> isRight $ validateVars t
  quickCheck $ \(RandomRedex r) -> isRight $ validateVars r

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
  let (Net reduced) = fixedPoint (reduceNet >>> substitute) net2
  let expected = (lambdaId "x") 
  quickCheck' 1 $ reduced.redexes == empty <?> show reduced <> " not simplified"
  quickCheck' 1 $ evalIso reduced.root expected <?> msg reduced.root expected

double :: VarLabel -> Net
double v = Net {
  root: lambdaNode v (makeOperator Add (Var $ v <> "a") (Var $ v <> "b")),
  redexes: [Redex (Var v) (makeDelta (Var $ v <> "a") (Var $ v <> "b"))]
  }

assign :: VarLabel -> Tree -> Redex
assign v t = Redex (Var v) t

applyNet :: (Tree -> Redex) -> Net -> Net -> Net
applyNet f (Net s) n = over _redexes (_ <> s.redexes <> [f s.root]) n

testMath :: Effect Unit
testMath = do
  let net =
        applyNet (applyNode (makeNumber 3.0) "y") (double "z") $
        applyNet (applyNode (Var "y") "out") (double "x") $
        Net {
          root: Var "out",
          redexes: []
        }
  let validated = validateNetVars net
  logShow net
  quickCheck' 1 $ (isRight validated) <?> "Invalid net " <> show net <> " -- " <> show validated
  let reduced = (fixedPoint $ reduceNet >>> substitute) net
  let evaluated = (fixedPoint evalTree) <<< view _root $ reduced
  quickCheck' 1 $ makeNumber 12.0 == evaluated

lambdaNode :: VarLabel -> Tree -> Tree
lambdaNode v = makeGamma (Var v)

applyNode :: Tree -> VarLabel -> Tree -> Redex
applyNode arg outVar f = Redex f (makeGamma arg (Var outVar))

lambdaId :: VarLabel -> Tree
lambdaId s = makeGamma (Var s) (Var s)

lambdaApplySelf :: Tree
lambdaApplySelf = makeGamma
  (makeDelta (Var "x1") (makeGamma (Var "x1") (Var "bod")))
  (Var "bod")

net2 :: Net
net2 = Net {root: (Var "root"), redexes: [Redex lambdaApplySelf (makeGamma (lambdaId "a") (Var "root"))]}

