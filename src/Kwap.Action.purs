module Kwap.Action where

import Prelude
import Data.Foldable (class Foldable, foldl)

data Action
  = Nop

fromFoldable :: ∀ f. Foldable f => f Action -> Action
fromFoldable = foldl (const identity) Nop
