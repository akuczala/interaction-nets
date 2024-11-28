module Net.Parser
  ( parseTree
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List.Lazy (take)
import Data.List.Lazy as L
import Data.List.NonEmpty (NonEmptyList, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Net.Redex (Redex, RedexF(..))
import Net.Tree (BinaryNode(..), NullaryNode(..), Pair, Tree, TreeF(..), TreePair, makeGamma, makeNumber, makePair)
import StringParser (ParseError, Parser, anyDigit, anyLetter, char, choice, fail, many1, runParser, sepBy, whiteSpace, (<?>))
import StringParser as P
import StringParser.Combinators (many1)

charsToString :: NonEmptyList Char -> String
charsToString = fromCharArray <<< toUnfoldable

number :: Parser Number
-- TODO implement
number = do
  x <- fromString <<< charsToString <$> many1 anyDigit <?> "number"
  case x of
    Just s -> pure s
    Nothing -> fail "Could not parse number"

parsePair :: forall a. Parser a -> Parser (Pair a)
parsePair p = do
  as <- p `sepBy` whiteSpace
  case as of
    (a1 : (a2 : _)) -> pure $ makePair a1 a2
    _ -> fail "Wrong number of arguments for pair"

atom :: Parser Tree
atom = fix $ \p -> choice
  [ Var <<< charsToString <$> many1 anyLetter
  , makeNumber <$> number
  , Nullary Epsilon <$ char '•'
  , Binary Gamma <$> parsePairBetween '(' ')' p
  , Binary Delta <$> parsePairBetween '{' '}' p
  ]
  where
  parsePairBetween :: Char -> Char -> Parser Tree -> Parser TreePair
  parsePairBetween open close p = P.between (char open) (char close) (parsePair p)

parseTree :: String -> Either ParseError Tree
parseTree = runParser atom