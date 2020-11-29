module Acronym (abbreviate) where

import Prelude
import Data.Array (take, filter)
import Data.Char.Unicode (isAlpha, isUpper, isLower, toUpper)
import Data.Foldable (all)
import Data.String (split, Pattern(..), joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)

acronym :: Array Char -> Array Char
acronym cs
  | all isUpper cs = take 1 cs
  | all isLower cs = toUpper <$> take 1 cs
  | otherwise = filter isUpper cs

abbreviate :: String -> String
abbreviate xs = joinWith "" $ fromCharArray <$> acronym <<< normalizeChars <<< toCharArray <$> words xs
  where
  words :: String -> Array String
  words = split (Pattern " ")

  normalizeChars :: Array Char -> Array Char
  normalizeChars cs = filter isAlpha cs
