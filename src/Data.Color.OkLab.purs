--| https://bottosson.github.io/posts/oklab/

module Data.Color.OkLab
  ( Lab(..)
  , L(..)
  , A(..)
  , B(..)
  , css
  , lch
  , lightness
  , chroma
  , hue
  , modifyLightness
  , setLightness
  , lighten
  , modifyChroma
  , setChroma
  , shiftChroma
  , modifyHue
  , setHue
  , shiftHue
  , toRgb
  , toCieXyz
  ) where

import Data.Fist
import Prelude

import CSS as Css
import CSS.Color as Css.Color
import Data.Color.Cie1931 as C
import Data.Color.Rgb as Rgb
import Data.Coord.Cart as Cartesian
import Data.Coord.Polar as Polar
import Data.Generic.Rep (class Generic)
import Data.Mat as Mat
import Data.Maybe (fromJust)
import Data.Number (pow)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype L = L Number

derive instance genericL :: Generic L _
instance showL :: Show L where
  show = genericShow

newtype A = A Number

derive instance genericA :: Generic A _
instance showA :: Show A where
  show = genericShow

newtype B = B Number

derive instance genericB :: Generic B _
instance showB :: Show B where
  show = genericShow

data Lab = Lab L A B

derive instance genericLab :: Generic Lab _
instance showLab :: Show Lab where
  show = genericShow

polar :: Lab -> Polar.Pos
polar (Lab _ (A a) (B b)) = Cartesian.toPolar $
  (Cartesian.make a b :: Cartesian.Pos)

ofPolar :: Number -> Polar.Pos -> Lab
ofPolar l p =
  let
    get :: (Cartesian.Pos -> Number) -> Number
    get n = n <<< Cartesian.fromPolar $ p
  in
    Lab (L l) (A $ get Cartesian.x) (B $ get Cartesian.y)

modifyLightness :: (Number -> Number) -> Lab -> Lab
modifyLightness f (Lab (L l) a b) = Lab (L $ f l) a b

setLightness :: Number -> Lab -> Lab
setLightness = const >>> modifyLightness

lighten :: Number -> Lab -> Lab
lighten dl l = setLightness (lightness l + dl) l

modifyChroma :: (Number -> Number) -> Lab -> Lab
modifyChroma f l = ofPolar (lightness l) <<< Polar.modifyRadius f <<< polar $ l

setChroma :: Number -> Lab -> Lab
setChroma = const >>> modifyChroma

shiftChroma :: Number -> Lab -> Lab
shiftChroma dc l = setChroma (chroma l + dc) l

modifyHue :: (Polar.Radians -> Polar.Radians) -> Lab -> Lab
modifyHue f l = ofPolar (lightness l) <<< Polar.modifyAngle f <<< polar $ l

setHue :: Polar.Radians -> Lab -> Lab
setHue = const >>> modifyHue

shiftHue :: Polar.Radians -> Lab -> Lab
shiftHue dh l = setHue (hue l + dh) l

lightness :: Lab -> Number
lightness (Lab (L l) _ _) = l

chroma :: Lab -> Number
chroma = Polar.radius <<< polar

hue :: Lab -> Polar.Radians
hue = Polar.angle <<< polar

lch :: forall a. Polar.Angle a => Number -> Number -> a -> Lab
lch l c h = ofPolar l (Polar.make c (Polar.toRadians h))

m1 :: Mat.M3x3 Number
m1 = Mat.M3x3
  0.8189330101
  0.3618667424
  (-0.1288597137)
  0.0329845436
  0.9293118715
  0.0361456387
  0.0482003018
  0.2643662691
  0.6338517070

m2 :: Mat.M3x3 Number
m2 = Mat.M3x3
  0.2104542553
  0.7936177850
  (-0.0040720468)
  1.9779984951
  (-2.4285922050)
  0.4505937099
  0.0259040371
  0.7827717662
  (-0.8086757660)

m1i :: Mat.M3x3 Number
m1i = unsafePartial fromJust $ Mat.inverse3x3 m1

m2i :: Mat.M3x3 Number
m2i = unsafePartial fromJust $ Mat.inverse3x3 m2

toCieXyz :: Lab -> C.Xyz
toCieXyz (Lab (L l) (A a) (B b)) =
  let
    lms = m2i `Mat.mul3x3_1x3` (Mat.M1x3 l a b)
    xyz = case lms of
      Mat.M1x3 l' m' s' -> m1i `Mat.mul3x3_1x3`
        (Mat.M1x3 (pow l' 3.0) (pow m' 3.0) (pow s' 3.0))
  in
    case xyz of
      Mat.M1x3 x y z -> C.Xyz (C.X x) (C.Y y) (C.Z z)

toRgb :: Lab -> Rgb.Rgb
toRgb = toCieXyz >>> C.toRgb

css :: Lab -> Css.Color.Color
css = toRgb >>> (case _ of (Rgb.Rgb (Rgb.R r) (Rgb.G g) (Rgb.B b)) -> Css.Color.rgb' r g b)
