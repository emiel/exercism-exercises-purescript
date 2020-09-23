module WordCount (wordCount) where

import Data.Map
import Data.Tuple (Tuple(..))
import Data.String (split, Pattern(..))

wordCount :: String -> Map String Int
wordCount phrase = fromFoldable [ Tuple "word" 3, Tuple "foo" 5 ]
  where
  words = split (Pattern " ") phrase
