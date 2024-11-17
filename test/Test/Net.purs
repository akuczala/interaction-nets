module Test.Net
  ( testNet
  )
  where

import Prelude

import Control.Monad.State (runState)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Nets (Redex(..), Tree(..), VarLabel, initReduceState, isomorphic, makeDelta, makeGamma, reduceArr, substitute)
import Random (randomTree, runRandom)
import Run as Run
import Run.State as Run.State
import Utils (fixedPoint)

testNet :: Effect Unit
testNet = do
    testReduce

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

