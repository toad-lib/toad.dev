module Kwap.Action where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Kwap.Navbar as Navbar

data Action
  = Nop
  | Init
  | NavbarSectionPicked Navbar.Section
  | Tick
  | DismissError

fromFoldable :: ∀ f. Foldable f => f Action -> Action
fromFoldable = foldl (const identity) Nop
