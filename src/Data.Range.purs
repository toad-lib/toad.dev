module Data.Range where

import Prelude

import Data.Maybe (Maybe(..))

class Ord x <= Bound b x where
  contains :: b -> x -> Boolean
  max :: b -> Maybe x
  min :: b -> Maybe x

data Range lower upper = Range lower upper
type RangeIncl a = Range (LowerIncl a) (UpperIncl a)
type RangeExcl a = Range (LowerIncl a) (UpperExcl a)

clamp :: ∀ a. Ord a => Range (LowerIncl a) (UpperIncl a) -> a -> a
clamp (Range lowerBound@(LowerIncl clampLow) upperBound@(UpperIncl clampHigh)) a =
  if not $ contains lowerBound a then clampLow
  else if not $ contains upperBound a then clampHigh
  else a

betweenExclusive :: ∀ a. Ord a => a -> a -> RangeExcl a
betweenExclusive low upp = Range (LowerIncl low) (UpperExcl upp)

betweenInclusive :: ∀ a. Ord a => a -> a -> RangeIncl a
betweenInclusive low upp = Range (LowerIncl low) (UpperIncl upp)

infixr 5 betweenExclusive as ..
infixr 5 betweenInclusive as ..=

data LowerIncl a = LowerIncl a
data LowerExcl a = LowerExcl a
data UpperIncl a = UpperIncl a
data UpperExcl a = UpperExcl a

instance rangeBound :: (Bound lo a, Bound hi a) => Bound (Range lo hi) a where
  contains (Range lo hi) x = contains lo x && contains hi x
  max (Range _ hi) = max hi
  min (Range lo _) = min lo

instance lowerInclBound :: Ord a => Bound (LowerIncl a) a where
  contains (LowerIncl b) x = x >= b
  max _ = Nothing
  min (LowerIncl b) = Just b

instance lowerExclBound :: Ord a => Bound (LowerExcl a) a where
  contains (LowerExcl b) x = x > b
  max _ = Nothing
  min (LowerExcl b) = Just b

instance upperInclBound :: Ord a => Bound (UpperIncl a) a where
  contains (UpperIncl b) x = x <= b
  max (UpperIncl b) = Just b
  min _ = Nothing

instance upperExclBound :: Ord a => Bound (UpperExcl a) a where
  contains (UpperExcl b) x = x < b
  max (UpperExcl b) = Just b
  min _ = Nothing
