module Test.Main
  ( main
  )
  where

import Prelude

import Control.Monad.State (runState)
import Data.Array (head)
import Data.Map as M
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Nets (Redex(..), Tree(..), VarLabel, isomorphic, makeDelta, makeGamma, reduceArr, substitute)
import Utils (fixedPoint)

main :: Effect Unit
main = do
  log "🍕"
  let reduced = fixedPoint (reduceArr >>> substitute) [redex2]
  logShow $ reduced
  logShow $ runState (
    isomorphic (
      simpleTree {a: "a", b: "b", c: "c", d: "b"}
      ) (
        simpleTree {a: "a", b: "d", c: "e", d: "d"}
        )
    ) M.empty

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
