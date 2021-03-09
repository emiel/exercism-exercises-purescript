module DifferenceOfSquares (differenceOfSquares, squareOfSum, sumOfSquares) where

import Prelude
import Data.Array (range)
import Data.Foldable (sum)
import Data.Int (pow)

squareOfSum :: Int -> Int
squareOfSum n = pow (sum $ range 1 n) 2

sumOfSquares :: Int -> Int
sumOfSquares n = sum $ map (\i -> pow i 2) (range 1 n)

differenceOfSquares :: Int -> Int
differenceOfSquares n = squareOfSum n - sumOfSquares n
