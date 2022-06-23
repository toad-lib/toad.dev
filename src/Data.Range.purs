module Data.Range where

import Prelude

class Ord x <= Bound b x where
  contains :: b -> x -> Boolean

data Range lower upper = Range lower upper

clamp :: ∀ a. Ord a => Range (LowerIncl a) (UpperIncl a) -> a -> a
clamp (Range lowerBound@(LowerIncl clampLow) upperBound@(UpperIncl clampHigh)) a =
  if not $ contains lowerBound a then clampLow
  else if not $ contains upperBound a then clampHigh
  else a

betweenExclusive :: ∀ a. Ord a => a -> a -> Range (LowerIncl a) (UpperExcl a)
betweenExclusive low upp = Range (LowerIncl low) (UpperExcl upp)

betweenInclusive :: ∀ a. Ord a => a -> a -> Range (LowerIncl a) (UpperIncl a)
betweenInclusive low upp = Range (LowerIncl low) (UpperIncl upp)

infixr 5 betweenExclusive as ..
infixr 5 betweenInclusive as ..=

data LowerIncl a = LowerIncl a
data LowerExcl a = LowerExcl a
data UpperIncl a = UpperIncl a
data UpperExcl a = UpperExcl a

instance rangeBound :: (Bound lo a, Bound hi a) => Bound (Range lo hi) a where
  contains (Range lo hi) x = contains lo x && contains hi x

instance lowerInclBound :: Ord a => Bound (LowerIncl a) a where
  contains (LowerIncl b) x = x >= b

instance lowerExclBound :: Ord a => Bound (LowerExcl a) a where
  contains (LowerExcl b) x = x > b

instance upperInclBound :: Ord a => Bound (UpperIncl a) a where
  contains (UpperIncl b) x = x <= b

instance upperExclBound :: Ord a => Bound (UpperExcl a) a where
  contains (UpperExcl b) x = x < b
