module Kwap.App.Action where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Kwap.App.Navbar as App.Navbar
import Kwap.App.Route as App.Route

data Action
  = Nop
  | Init
  | NavbarSectionPicked App.Navbar.Section
  | Tick

fromFoldable :: ∀ f. Foldable f => f Action -> Action
fromFoldable = foldl (const identity) Nop
