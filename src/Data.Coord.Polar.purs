module Data.Coord.Polar
  ( Radians(..)
  , Degrees(..)
  , class Polar
  , Pos
  , class Angle
  , toRadians
  , modifyAngle
  , modifyRadius
  , makePos
  , radius
  , angle
  , make
  , radiansToDegrees
  , degreesToRadians
  , radiansFloat
  , degreesFloat
  ) where

import Prelude

import Data.Number (pi, (%))
import Data.Tuple (Tuple(..))

newtype Radians = Radians Number

derive newtype instance showRadians :: Show Radians
derive newtype instance semiringRadians :: Semiring Radians
derive newtype instance ringRadians :: Ring Radians
derive newtype instance eqRadians :: Eq Radians
instance angleRadians :: Angle Radians where
  toRadians = identity

newtype Degrees = Degrees Number

derive newtype instance showDegrees :: Show Degrees
derive newtype instance semiringDegrees :: Semiring Degrees
derive newtype instance ringDegrees :: Ring Degrees
derive newtype instance eqDegrees :: Eq Degrees
instance angleDegrees :: Angle Degrees where
  toRadians = degreesToRadians

class Angle a where
  toRadians :: a -> Radians

radiansFloat :: Radians -> Number
radiansFloat (Radians n)
  | n < (2.0 * pi) = n
  | otherwise = n % (2.0 * pi)

degreesFloat :: Degrees -> Number
degreesFloat (Degrees n)
  | n < 360.0 = n
  | otherwise = n % 360.0

radiansToDegrees :: Radians -> Degrees
radiansToDegrees = Degrees <<< mul 360.0 <<< (_ / (2.0 * pi)) <<< radiansFloat

degreesToRadians :: Degrees -> Radians
degreesToRadians = Radians <<< mul (2.0 * pi) <<< (_ / 360.0) <<< degreesFloat

class Polar x where
  radius :: x -> Number
  angle :: x -> Radians
  make :: Number -> Radians -> x

newtype Pos = Pos (Tuple Number Radians)

derive newtype instance eqPos :: Eq Pos
derive newtype instance showPos :: Show Pos
instance polarPos :: Polar Pos where
  radius (Pos (Tuple r _)) = r
  angle (Pos (Tuple _ a)) = a
  make r a = Pos $ Tuple r a

makePos :: Number -> Radians -> Pos
makePos = make

modifyAngle :: (Radians -> Radians) -> Pos -> Pos
modifyAngle f p = make (radius p) (f $ angle p)

modifyRadius :: (Number -> Number) -> Pos -> Pos
modifyRadius f p = make (f $ radius p) (angle p)
