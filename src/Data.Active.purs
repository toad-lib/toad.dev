module Data.Active where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Active = Active | Inactive

derive instance genericActive :: Generic Active _
instance showActive :: Show Active where
  show = genericShow

derive instance eqActive :: Eq Active
