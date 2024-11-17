module Test.Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Random (testRandom)
import Test.Net (testNet)

main :: Effect Unit
main = do
  log "🍕"
  testRandom
  testNet
