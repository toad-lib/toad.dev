module Data.Color.Cie1931
  ( Xyz(..)
  , X(..)
  , Y(..)
  , Z(..)
  , ofRgb
  , toRgb
  , mXyzToLrgb
  ) where

import Prelude

import Data.Color.Rgb as Rgb
import Data.Generic.Rep (class Generic)
import Data.Mat as Mat
import Data.Maybe (fromJust)
import Data.Number (pow)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype X = X Number

derive instance genericX :: Generic X _
instance showX :: Show X where
  show = genericShow

newtype Y = Y Number

derive instance genericY :: Generic Y _
instance showY :: Show Y where
  show = genericShow

newtype Z = Z Number

derive instance genericZ :: Generic Z _
instance showZ :: Show Z where
  show = genericShow

data Xyz = Xyz X Y Z

derive instance genericXyz :: Generic Xyz _
instance showXyz :: Show Xyz where
  show = genericShow

--| https://en.wikipedia.org/wiki/CIE_1931_color_space#Construction_of_the_CIE_XYZ_color_space_from_the_Wright%E2%80%93Guild_data
mLrgbToXyz :: Mat.M3x3 Number
mLrgbToXyz = Mat.M3x3
  0.4124
  0.3576
  0.1805
  0.2126
  0.7152
  0.0722
  0.0193
  0.1192
  0.9505

mXyzToLrgb :: Mat.M3x3 Number
mXyzToLrgb = unsafePartial fromJust $ Mat.inverse3x3 mLrgbToXyz

lrgbToSrgb :: Rgb.Rgb -> Rgb.Rgb
--lrgbToSrgb = identity
lrgbToSrgb (Rgb.Rgb (Rgb.R r) (Rgb.G g) (Rgb.B b)) =
  let
    threshold = 0.0031308
    f n
      | n <= threshold = 12.92 * n
      | otherwise = (1.055 * (pow n (1.0 / 2.4))) - 0.055
  in
    Rgb.Rgb (Rgb.R $ f r) (Rgb.G $ f g) (Rgb.B $ f b)

srgbToLrgb :: Rgb.Rgb -> Rgb.Rgb
--srgbToLrgb = identity
srgbToLrgb (Rgb.Rgb (Rgb.R r) (Rgb.G g) (Rgb.B b)) =
  let
    threshold = 0.04045
    f n
      | n <= threshold = n / 12.92
      | otherwise = pow ((n + 0.055) / 1.055) 2.4
  in
    Rgb.Rgb (Rgb.R $ f r) (Rgb.G $ f g) (Rgb.B $ f b)

toRgb :: Xyz -> Rgb.Rgb
toRgb (Xyz (X x) (Y y) (Z z)) =
  let
    rgb = mXyzToLrgb `Mat.mul3x3_1x3` (Mat.M1x3 x y z)
  in
    case rgb of
      Mat.M1x3 r g b -> lrgbToSrgb $ Rgb.Rgb (Rgb.R r) (Rgb.G g) (Rgb.B b)

ofRgb :: Rgb.Rgb -> Xyz
ofRgb rgb =
  let
    xyz = case srgbToLrgb rgb of
      (Rgb.Rgb (Rgb.R r) (Rgb.G g) (Rgb.B b)) -> mLrgbToXyz `Mat.mul3x3_1x3`
        (Mat.M1x3 r g b)
  in
    case xyz of Mat.M1x3 x y z -> Xyz (X x) (Y y) (Z z)
