module Data.Mat where

import Data.Fist
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data M1x3 a = M1x3 a a a

derive instance genericM1x3 :: Generic (M1x3 a) _
instance showM1x3 :: Show a => Show (M1x3 a) where
  show = genericShow

data M2x2 a = M2x2 a a a a

derive instance genericM2x2 :: Generic (M2x2 a) _
instance showM2x2 :: Show a => Show (M2x2 a) where
  show = genericShow

data M3x3 a = M3x3 a a a a a a a a a

derive instance genericM3x3 :: Show a => Generic (M3x3 a) _
instance showM3x3 :: Show a => Show (M3x3 a) where
  show = genericShow

mul3x3_3x3 :: forall a. M3x3 a -> M3x3 a -> M3x3 a
mul3x3_3x3 _ = identity

mul3x3_1x3 :: forall a. Semiring a => M3x3 a -> M1x3 a -> M1x3 a
mul3x3_1x3
  ( M3x3
      a11
      a12
      a13
      a21
      a22
      a23
      a31
      a32
      a33
  )
  (M1x3 b11 b21 b31) = M1x3
  ((a11 * b11) + (a12 * b21) + (a13 * b31))
  ((a21 * b11) + (a22 * b21) + (a23 * b31))
  ((a31 * b11) + (a32 * b21) + (a33 * b31))

mulScalar3x3 :: forall a. Semiring a => a -> M3x3 a -> M3x3 a
mulScalar3x3 x (M3x3 a b c d e f g h i) = M3x3 (a * x) (b * x) (c * x) (d * x)
  (e * x)
  (f * x)
  (g * x)
  (h * x)
  (i * x)

det2x2 :: forall a. Ring a => M2x2 a -> a
det2x2 (M2x2 a b c d) = (a * d) - (b * c)

det2x2' :: forall a. Ring a => a -> a -> a -> a -> a
det2x2' a b c d = det2x2 $ M2x2 a b c d

det3x3 :: forall a. Ring a => M3x3 a -> a
det3x3 (M3x3 a b c d e f g h i) = (a * (det2x2' e f h i))
  - (b * (det2x2' d f g i))
  + (c * (det2x2' d e g h))

transpose3x3 :: forall a. M3x3 a -> M3x3 a
transpose3x3 (M3x3 a b c d e f g h i) = M3x3 a d g b e h c f i

cofactor3x3 :: forall a. Ring a => M3x3 a -> M3x3 a
cofactor3x3
  ( M3x3
      a11
      a12
      a13
      a21
      a22
      a23
      a31
      a32
      a33
  ) = M3x3
  (det2x2' a22 a23 a32 a33)
  (negate $ det2x2' a21 a23 a31 a33)
  (det2x2' a21 a22 a31 a32)
  (negate $ det2x2' a12 a13 a32 a33)
  (det2x2' a11 a13 a31 a33)
  (negate $ det2x2' a11 a12 a31 a32)
  (det2x2' a12 a13 a22 a23)
  (negate $ det2x2' a11 a13 a21 a23)
  (det2x2' a11 a12 a21 a22)

adjugate3x3 :: forall a. Ring a => M3x3 a -> M3x3 a
adjugate3x3 = cofactor3x3 >>> transpose3x3

inverse3x3 :: forall a. Eq a => EuclideanRing a => M3x3 a -> Maybe (M3x3 a)
inverse3x3 m
  | det3x3 m == zero = Nothing
  | otherwise = Just $ (one / det3x3 m) `mulScalar3x3` adjugate3x3 m
