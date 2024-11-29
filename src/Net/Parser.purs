module Net.Parser
  ( parseRedex
  , parseTree
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Either (Either)
import Data.List ((:))
import Data.List.NonEmpty (NonEmptyList, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Net.Redex (RedexF(..), Redex)
import Net.Tree (BinaryNode(..), NullaryNode(..), Operator(..), Pair, Tree, TreeF(..), TreePair, makeNumber, makePair)
import StringParser (ParseError, Parser, anyDigit, anyLetter, char, choice, fail, oneOf, runParser, sepBy, skipSpaces, whiteSpace, (<?>))
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

operator :: forall a. Char -> Parser a -> Parser a -> Parser (Pair a)
operator op p1 p2 = do
  skipSpaces
  a1 <- p1
  skipSpaces
  _ <- char op
  skipSpaces
  a2 <- p2
  pure $ makePair a1 a2

parseOp :: Parser Operator
parseOp = do
  c <- oneOf [ '+', '-', '*', '/' ]
  case c of
    '+' -> pure Add
    '-' -> pure Sub
    '*' -> pure Mul
    '/' -> pure Div
    x -> fail $ "Invalid operator " <> show x

atom :: Parser Tree
atom = fix $ \p -> choice
  [ Var <<< charsToString <$> many1 anyLetter
  , makeNumber <$> number
  , Nullary Epsilon <$ char '•'
  , Binary Gamma <$> parsePairBetween '(' ')' p
  , Binary Delta <$> parsePairBetween '{' '}' p
  , parseOperator p
  ]
  where
  parsePairBetween :: Char -> Char -> Parser Tree -> Parser TreePair
  parsePairBetween open close p = P.between (char open) (char close) (parsePair p)
  parseOperator p = (\(Tuple op pair) -> Binary (Operator op) pair) <$> P.between (char '[') (char ']') opParse
    where
    opParse = do
      t1 <- p
      skipSpaces
      op <- parseOp
      skipSpaces
      t2 <- p
      pure $ Tuple op (makePair t1 t2)

parseTree :: String -> Either ParseError Tree
parseTree = runParser atom

redex :: Parser Redex
redex = do
  pair <- operator '~' atom atom
  pure $ Redex pair.fst pair.snd

parseRedex :: String -> Either ParseError Redex
parseRedex = runParser redex