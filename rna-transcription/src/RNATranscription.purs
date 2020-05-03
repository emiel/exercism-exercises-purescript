module RNATranscription
  ( toRNA
  ) where

import Prelude
import Data.String.CodeUnits
import Data.Maybe

transcribe :: Char -> Maybe Char
transcribe c = case c of
  'A' -> Just 'U'
  'C' -> Just 'G'
  'G' -> Just 'C'
  'T' -> Just 'A'
  _ -> Nothing

-- toRNA :: String -> Maybe String
toRNA str = (toCharArray str) >>= transcribe
