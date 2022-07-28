module Toad.Css.Color
  ( Color(..)
  , color
  ) where

import Prelude

import CSS.Color as Css.Color
import Data.Generic.Rep (class Generic)
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

hue :: Color -> Number
hue = case _ of
  -- TODO: get hues
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
