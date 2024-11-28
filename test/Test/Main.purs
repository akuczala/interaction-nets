module Test.Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Net (testNet)
import Test.Parser (testParser)
import Test.Random (testRandom)

main :: Effect Unit
main = do
  log "🍕"
  testRandom
  testNet
  testParser
