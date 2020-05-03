module Leap where

import Prelude

isLeapYear :: Int -> Boolean
isLeapYear year
  | year `mod` 4 /= 0 = false
  | year `mod` 100 /= 0 = true
  | year `mod` 400 /= 0 = false
  | otherwise = true
