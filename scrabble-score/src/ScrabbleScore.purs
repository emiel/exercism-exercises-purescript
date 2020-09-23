module ScrabbleScore (scoreWord) where

import Prelude
import Data.Char.Unicode (toLower)
import Data.Foldable (elem, sum)
import Data.String.CodeUnits (toCharArray)

scoreLetter :: Char -> Int
scoreLetter l
  | (elem <<< toLower) l [ 'a', 'e', 'i', 'o', 'u', 'l', 'n', 'r', 's', 't' ] = 1
  | (elem <<< toLower) l [ 'd', 'g' ] = 2
  | (elem <<< toLower) l [ 'b', 'c', 'm', 'p' ] = 3
  | (elem <<< toLower) l [ 'f', 'h', 'v', 'w', 'y' ] = 4
  | toLower l == 'k' = 5
  | (elem <<< toLower) l [ 'j', 'x' ] = 8
  | (elem <<< toLower) l [ 'q', 'z' ] = 10
  | otherwise = 0

scoreWord :: String -> Int
scoreWord word = sum $ scoreLetter <$> toCharArray word
