module Toad.Action where

import Toad.Prelude

import Toad.Atom.AppTitle (AppTitle)
import Toad.Page.Concepts as Page.Concepts
import Toad.Route as Route

data Action
  = Nop
  | Init
  | AppTitleChanged AppTitle
  | NavbarSectionPicked
  | Navigate Route.Route
  | Tick
  | DismissError
  | ConceptsPageOutput Page.Concepts.Output
  | NavAccordionExpandBook
  | NavAccordionExpandConcepts

fromFoldable :: ∀ f. Foldable f => f Action -> Action
fromFoldable = foldl (const identity) Nop
