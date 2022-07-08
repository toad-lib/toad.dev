module Kwap.Navigate where

import Prelude

import Halogen (HalogenM)
import Halogen as H
import Kwap.App.Route as App.Route

class Monad m <= Navigate m where
  navigate :: App.Route.Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM a b c d m) where
  navigate = H.lift <<< navigate
