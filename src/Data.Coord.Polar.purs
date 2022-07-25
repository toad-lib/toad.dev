module Data.Coord.Polar where

import Prelude

import Data.Tuple (Tuple(..))

newtype Radians = Radians Number

derive newtype instance showRadians :: Show Radians
derive newtype instance semiringRadians :: Semiring Radians
derive newtype instance ringRadians :: Ring Radians
derive newtype instance eqRadians :: Eq Radians

rad :: Radians -> Number
rad (Radians n) = n

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
