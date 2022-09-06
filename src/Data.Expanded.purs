module Data.Expanded where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Expanded = Expanded | Collapsed

derive instance genericExpanded :: Generic Expanded _
instance showExpanded :: Show Expanded where
  show = genericShow

derive instance eqExpanded :: Eq Expanded

toggle :: Expanded -> Expanded
toggle Expanded = Collapsed
toggle Collapsed = Expanded
