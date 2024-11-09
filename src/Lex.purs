module Lex
  ( Lex(..)
  , charSucc
  , succ
  , unLex
  )
  where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.CodeUnits (charAt, singleton, uncons)

newtype Lex = Lex String

instance Eq Lex where
    eq (Lex x) (Lex y) = eq x y

instance Ord Lex where
    compare (Lex x) (Lex y) = compare x y

charSucc :: Char -> Maybe Char
charSucc 'z' = Nothing
charSucc c
    = c # toCharCode
    >>> (\y -> y + 1)
    >>> fromCharCode


-- succ :: String -> String
-- succ s =
--     s # charAt 0
--     >>> fromMaybe 'a'
--     >>> charSucc
--     >>> stuff
--     >>> Lex
--     where
--     {head, tail} = fromMaybe uncons s
--     stuff Nothing = s <> "a"
--     stuff (Just c) = s <> singleton c

succ :: String -> String
succ "" = "a"
succ s = case charSucc head of
    Nothing -> "a" <> s
    Just c -> singleton c <> tail
    where
    {head, tail} = fromMaybe {head: '!', tail: ""} (uncons s)

unLex (Lex x) = x