module SumOfMultiples (sumOfMultiples) where

import Prelude
import Data.Array (filter, (..))
import Data.Foldable (sum, any)

isMultipleOfAny :: Array Int -> Int -> Boolean
isMultipleOfAny xs n = any (isMultiple n) xs
  where
  isMultiple x y = x `mod` y == 0

sumOfMultiples :: Array Int -> Int -> Int
sumOfMultiples xs n = sum $ filter (isMultipleOfAny xs) (0 .. (n - 1))
