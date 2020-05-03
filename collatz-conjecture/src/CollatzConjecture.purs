module CollatzConjecture
  ( collatz
  ) where

import Data.Int (even)
import Data.Maybe (Maybe(..))
import Prelude

collatz :: Int -> Maybe Int
collatz = go 0
  where
  go acc n
    | n <= 0 = Nothing
    | n == 1 = Just acc
    | even n = go (acc + 1) (n / 2)
    | otherwise = go (acc + 1) ((3 * n) + 1)
