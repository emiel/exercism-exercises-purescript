module RNATranscription (toRNA) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Traversable (traverse)

type Nucleotide
  = Char

type DNA
  = Array Nucleotide

type RNA
  = Array Nucleotide

complement :: Nucleotide -> Maybe Nucleotide
complement n = case n of
  'A' -> Just 'U'
  'C' -> Just 'G'
  'G' -> Just 'C'
  'T' -> Just 'A'
  _ -> Nothing

toRNA :: String -> Maybe String
toRNA strand = fromCharArray <$> transcribe (toCharArray strand)
  where
  transcribe :: DNA -> Maybe RNA
  transcribe = traverse complement
