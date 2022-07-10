module Kwap.App.Navbar.Section (Section(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Section = Home | Book | Concepts

derive instance eqSection :: Eq Section
derive instance genericSection :: Generic Section _
instance showSection :: Show Section where
  show = genericShow
