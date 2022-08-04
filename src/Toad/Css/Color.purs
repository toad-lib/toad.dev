module Toad.Css.Color
  ( green
  , grey
  , colorBg
  , colorBg2
  , colorFg
  , colorPrimary
  , colorPrimary2
  ) where

import Toad.Prelude

import Data.Color.OkLab as OkLab
import Data.Newtype (unwrap)
import Data.Number (pow)
import Data.Range (contains, (..))

grey :: OkLab.Lightness -> OkLab.Lab
grey l = OkLab.lch (unwrap l) 0.0 (Degrees 0.0)

green :: OkLab.Lightness -> OkLab.Lab
green l =
  let
    --| https://www.desmos.com/calculator/ld3oi1ivvh
    c l'
      | contains (0.0 .. 0.87) l' = 0.37 * (pow l' 2.0)
      | otherwise = 16.0 * (pow (1.0 - l') 2.0)
  in
    OkLab.lch (unwrap l) (c $ unwrap l) (Degrees 142.0)

colorBg :: (OkLab.Lightness -> OkLab.Lab) -> OkLab.Lab
colorBg f = f (OkLab.Lightness 0.0)

colorBg2 :: (OkLab.Lightness -> OkLab.Lab) -> OkLab.Lab
colorBg2 f = f (OkLab.Lightness 0.2)

colorFg :: (OkLab.Lightness -> OkLab.Lab) -> OkLab.Lab
colorFg f = f (OkLab.Lightness 0.99)

colorPrimary :: (OkLab.Lightness -> OkLab.Lab) -> OkLab.Lab
colorPrimary f = f (OkLab.Lightness 0.45)

colorPrimary2 :: (OkLab.Lightness -> OkLab.Lab) -> OkLab.Lab
colorPrimary2 f = f (OkLab.Lightness 0.55)
