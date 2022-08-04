module Toad.Action where

import Toad.Prelude

import Toad.Page.Concepts as Page.Concepts

data Action
  = Nop
  | Init
  | NavbarSectionPicked
  | Tick
  | DismissError
  | ConceptsPageOutput Page.Concepts.Output

fromFoldable :: ∀ f. Foldable f => f Action -> Action
fromFoldable = foldl (const identity) Nop
