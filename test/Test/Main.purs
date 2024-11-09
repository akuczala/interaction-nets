module Test.Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Lex (Lex(..), succ, unLex)
import Nets (Redex(..), Tree(..), fixedPoint, makeDelta, makeGamma, reduce, reduceAll, reduceArr, substitute)

main :: Effect Unit
main = do
  log "🍕"
  logShow $ fixedPoint (reduceArr >>> substitute) [redex2]

simpleRedex :: Redex
simpleRedex = (Redex (makeGamma (Var "a") (Var "b")) (makeDelta (Var "c") (Var "d")))

lambdaId :: Tree
lambdaId = makeGamma (Var "a") (Var "a")

lambdaApplySelf :: Tree
lambdaApplySelf = makeGamma (makeDelta (Var "x1") (makeGamma (Var "x1") (Var "bod"))) (Var "bod")

redex2 :: Redex
redex2 = Redex lambdaApplySelf (makeGamma lambdaId (Var "root"))
-- succ :: Tree
-- succ = makeGamma (
--     makeDelta (makeGamma (Var "a") (Var "b")) (makeGamma (Var "b") (Var "R"))
--     ) (makeGamma (Var "a") (Var "R"))