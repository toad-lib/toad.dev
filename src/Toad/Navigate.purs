module Toad.Navigate where

import Prelude

import Halogen (HalogenM)
import Halogen as H
import Toad.Route as Route

class Monad m <= Navigate m where
  navigate :: Route.Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM a b c d m) where
  navigate = H.lift <<< navigate
