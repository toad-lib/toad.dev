module Toad.Css.Color
  ( Color(..)
  , color
  , green
  ) where

import Prelude

import CSS.Color as Css.Color
import Data.Color.OkLab as OkLab
import Data.Coord.Polar (Degrees(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Number (pow)
import Data.Range (contains, (..))
import Data.Show.Generic (genericShow)

data Color
  = Blue
  | Yellow
  | Pink
  | Black
  | White

derive instance eqColor :: Eq Color
derive instance genericColor :: Generic Color _
instance showColor :: Show Color where
  show = genericShow

green :: OkLab.Lightness -> OkLab.Lab
green l =
  let
    --| https://www.desmos.com/calculator/ld3oi1ivvh
    c l'
      | contains (0.0 .. 0.87) l' = 0.37 * (pow l' 2.0)
      | otherwise = 16.0 * (pow (1.0 - l') 2.0)
  in
    OkLab.lch (unwrap l) (c $ unwrap l) (Degrees 142.0)

hue :: Color -> Number
hue = case _ of
  Yellow -> 61.0
  Blue -> 180.0
  Pink -> 321.0
  _ -> 0.0

sat :: Color -> Number
sat = case _ of
  -- TODO: get sats
  Yellow -> 100.0
  Blue -> 100.0
  Pink -> 85.0
  _ -> 0.0

light :: Color -> Number
light = case _ of
  -- TODO: get lights
  Yellow -> 70.0
  Pink -> 75.0
  Blue -> 59.0
  White -> 100.0
  Black -> 0.0

color :: Color -> Css.Color.Color
color color' = Css.Color.hsla (hue color') (sat color') (light color') 1.0
