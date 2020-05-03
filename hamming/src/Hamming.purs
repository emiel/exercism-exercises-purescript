module Hamming (distance) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (zipWith)
import Data.Foldable (sum)
import Data.String (toCodePointArray)
import Data.String.CodePoints as CP

distance :: String -> String -> Maybe Int
distance seq1 seq2
  | CP.length seq1 /= CP.length seq2 = Nothing
  | otherwise = Just $ sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys
    where
    xs = toCodePointArray seq1
    ys = toCodePointArray seq2
