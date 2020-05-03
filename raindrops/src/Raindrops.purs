module Raindrops (raindrops) where

import Prelude
import Data.Array (filter, intercalate)
import Data.Int (decimal, toStringAs)

raindrops :: Int -> String
raindrops n = case factors of
  [] -> toStringAs decimal n
  _ -> intercalate "" $ map raindrop factors
  where
  factors = filter (\x -> n `mod` x == 0) [ 3, 5, 7 ]
  raindrop d = case d of
    3 -> "Pling"
    5 -> "Plang"
    7 -> "Plong"
    _ -> ""
