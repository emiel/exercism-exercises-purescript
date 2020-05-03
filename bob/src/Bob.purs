module Bob (hey) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (length, lastIndexOf, Pattern(..))
import Data.String.CodeUnits (toCharArray)
import Data.Char.Unicode (isLetter, isSpace, isUpper)
import Data.Foldable (all, any)
import Data.Array (filter)

isQuestion :: String -> Boolean
isQuestion str = case (lastIndexOf (Pattern "?") str) of
  Nothing -> false
  (Just n) -> n == length str - 1

isYell :: String -> Boolean
isYell str = hasLetter str && allUpper str
  where
  hasLetter s = any isLetter (toCharArray s)
  allUpper s = all isUpper (filter isLetter (toCharArray s))

isSilence :: String -> Boolean
isSilence str = all isSpace (toCharArray str)

hey :: String -> String
hey s
  | isYell s = "Whoa, chill out!"
  | isQuestion s = "Sure."
  | isSilence s = "Fine. Be that way!"
  | otherwise = "Whatever."
