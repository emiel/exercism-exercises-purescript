module Acronym (abbreviate) where

import Prelude

import Data.String (Pattern(..), split)
import Data.String.CodeUnits (charAt, fromCharArray)

-- abbreviate :: String -> String
abbreviate s =
  let
    words = split (Pattern " ") s
  in
    ?asdf (charAt 1) <$> words
