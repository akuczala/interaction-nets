module Lex
  ( charSucc
  , succ
  ) where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton, uncons)

charSucc :: Char -> Maybe Char
charSucc 'z' = Nothing
charSucc c = c # toCharCode
  >>> (\y -> y + 1)
  >>> fromCharCode

succ :: String -> String
succ "" = "a"
succ s = case charSucc head of
  Nothing -> "a" <> s
  Just c -> singleton c <> tail
  where
  { head, tail } = fromMaybe { head: '!', tail: "" } (uncons s)