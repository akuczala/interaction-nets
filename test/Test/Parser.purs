module Test.Parser
  ( testParser
  )
  where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Net.Parser (parseRedex, parseTree)
import Test.Net (RandomRedex(..), RandomTree(..))
import Test.QuickCheck (quickCheck, (<?>))

testParser :: Effect Unit
testParser = do
  testParseTree
  testParseRedex

testParseTree :: Effect Unit
testParseTree = do
  quickCheck $ \(RandomTree t) ->
    let parsed = parseTree (show t)
    in
      parsed == Right t
      <?> "Parsed tree " <> show parsed <> " doesn't match original " <> show t

testParseRedex :: Effect Unit
testParseRedex = do
  quickCheck $ \(RandomRedex r) ->
    let parsed = parseRedex (show r)
    in
      parsed == Right r
      <?> "Parsed redex " <> show parsed <> " doesn't match original " <> show r