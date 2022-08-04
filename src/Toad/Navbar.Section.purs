module Toad.Navbar.Section (Section(..)) where

import Toad.Prelude

data Section = Home | Book | Concepts

derive instance eqSection :: Eq Section
derive instance genericSection :: Generic Section _
instance showSection :: Show Section where
  show = genericShow
