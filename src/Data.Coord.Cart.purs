module Data.Coord.Cart where

import Prelude

import Data.Coord.Polar (class Polar, Radians(..), angle, radiansFloat, radius)
import Data.Coord.Polar as Polar
import Data.Generic.Rep (class Generic)
import Data.Number (atan2, cos, pow, sin, sqrt)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd, swap)
import Data.Tuple.Nested ((/\))

class Cartesian x where
  coords :: x -> Tuple Number Number
  fromCoords :: Tuple Number Number -> x

x :: forall f. Cartesian f => f -> Number
x = coords >>> fst

y :: forall f. Cartesian f => f -> Number
y = coords >>> snd

fromPolar :: forall p c. Polar p => Cartesian c => p -> c
fromPolar p = make (radius p * (cos <<< radiansFloat <<< angle $ p))
  (radius p * (sin <<< radiansFloat <<< angle $ p))

toPolar :: forall p c. Polar p => Cartesian c => c -> p
toPolar f =
  let
    toPolar' (Tuple x' y') = Polar.make (magnitude f) (Radians $ atan2 y' x')
  in
    toPolar' $ coords f

make :: forall f. Cartesian f => Number -> Number -> f
make x y = fromCoords $ Tuple x y

newtype Pos = Pos (Tuple Number Number)

derive newtype instance eqPos :: Eq Pos
instance showPos :: Show Pos where
  show = genericShow

derive instance genericPos :: Generic Pos _
instance cartesianPos :: Cartesian Pos where
  coords (Pos t) = t
  fromCoords t = Pos t

newtype Velocity = Velocity (Tuple Number Number)

derive newtype instance eqVelocity :: Eq Velocity
instance showVelocity :: Show Velocity where
  show = genericShow

derive instance genericVelocity :: Generic Velocity _
instance cartesianVelocity :: Cartesian Velocity where
  coords (Velocity t) = t
  fromCoords t = Velocity t

newtype Accel = Accel (Tuple Number Number)

derive newtype instance eqAccel :: Eq Accel
instance showAccel :: Show Accel where
  show = genericShow

derive instance genericAccel :: Generic Accel _
instance cartesianAccel :: Cartesian Accel where
  coords (Accel t) = t
  fromCoords t = Accel t

integral :: ∀ df f. Cartesian df => Cartesian f => df -> f -> f
integral df f =
  let
    df' = coords df
    f' = coords f
    go (Tuple dx dy) (Tuple x y) = fromCoords $ (dx + x) /\ (dy + y)
  in
    go df' f'

magnitude :: forall f. Cartesian f => f -> Number
magnitude = (\(Tuple a b) -> sqrt $ pow a 2.0 + pow b 2.0) <<< coords

normalize :: forall f. Cartesian f => f -> f
normalize f = mulBy (1.0 / magnitude f) f

mulBy :: forall f. Cartesian f => Number -> f -> f
mulBy k = fromCoords <<< (\(Tuple x y) -> Tuple (x * k) (y * k)) <<< coords
