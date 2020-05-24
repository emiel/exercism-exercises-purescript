module Triangle (triangleKind, Triangle(..)) where

import Prelude
import Data.Either (Either(..))
import Data.Ord (abs)

type TriangleSides
  = { a :: Int, b :: Int, c :: Int }

data Triangle
  = Equilateral
  | Isosceles
  | Scalene

derive instance eqTriangle :: Eq Triangle

instance showTriangle :: Show Triangle where
  show Equilateral = "Equilateral"
  show Isosceles = "Isosceles"
  show Scalene = "Scalene"

triangleHasSize :: TriangleSides -> Either String TriangleSides
triangleHasSize t@{ a, b, c } =
  if a == 0 && b == 0 && c == 0 then
    Left "Invalid lengths"
  else
    Right t

trianglePositive :: TriangleSides -> Either String TriangleSides
trianglePositive t@{ a, b, c } =
  if a < 0 || b < 0 || c < 0 then
    Left "Invalid lengths"
  else
    Right t

triangleMeetsInequality :: TriangleSides -> Either String TriangleSides
triangleMeetsInequality t@{ a, b, c } =
  if not $ abs (a - b) < c && c < (a + b) then
    Left "Violates inequality"
  else
    Right t

isEquilateral :: TriangleSides -> Boolean
isEquilateral t@{ a, b, c } = a == b && a == c && b == c

isIsosceles :: TriangleSides -> Boolean
isIsosceles t@{ a, b, c } = a == b || a == c || b == c

isScalene :: TriangleSides -> Boolean
isScalene t@{ a, b, c } = a /= b && a /= c && b /= c

isTriangle :: TriangleSides -> Either String TriangleSides
isTriangle t = (triangleHasSize t >>= trianglePositive) >>= triangleMeetsInequality

triangleKind :: Int -> Int -> Int -> Either String Triangle
triangleKind a b c = case isTriangle { a, b, c } of
  Left err -> Left err
  Right t ->
    if isEquilateral t then
      Right Equilateral
    else
      if isIsosceles t then
        Right Isosceles
      else
        if isScalene t then
          Right Scalene
        else
          Left "Unknown triangle kind"
