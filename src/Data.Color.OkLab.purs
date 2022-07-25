module Data.Color.OkLab
  ( Lab(..)
  , L(..)
  , A(..)
  , B(..)
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
  ) where

import Prelude

import Data.Color.Rgb as Rgb
import Data.Coord.Cart as Cartesian
import Data.Coord.Polar as Polar
import Data.Number (pow)

newtype L = L Number
newtype A = A Number
newtype B = B Number

data Lab = Lab L A B

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

lch :: Number -> Number -> Polar.Radians -> Lab
lch l c h = ofPolar l (Polar.make c h)

--| https://bottosson.github.io/posts/oklab/
toRgb :: Lab -> Rgb.Rgb
toRgb (Lab (L l') (A a) (B b)) =
  let
    -- THE NUMBERS, WHAT DO THEY MEAN!?
    pow' = flip pow
    l = pow' 3.0 $ l' + (0.3963377774 * a) + (0.2158037573 * b)
    m = pow' 3.0 $ l' - (0.1055613458 * a) - (0.0638541728 * b)
    s = pow' 3.0 $ l' - (0.0894841775 * a) - (1.2914855480 * b)
    red = (4.0767416621 * l) - (3.3077115913 * m) + (0.2309699292 * s)
    grn = (-1.2684380046 * l) + (2.6097574011 * m) - (0.3413193965 * s)
    blu = (-0.0041960863 * l) - (0.7034186147 * m) + (1.7076147010 * s)
  in
    Rgb.Rgb (Rgb.R red) (Rgb.G grn) (Rgb.B blu)
