module Test.Parser
  ( testParser
  )
  where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Net.Parser (parseTree)
import Test.Net (RandomTree(..))
import Test.QuickCheck (quickCheck, (<?>))

testParser :: Effect Unit
testParser = do
  quickCheck $ \(RandomTree r) ->
    let parsed = parseTree (show r)
    in
      parsed == Right r <?> "Parsed tree " <> show parsed <> " doesn't match original " <> show r