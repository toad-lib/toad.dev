module Kwap.App.Query (Query(..), sendNavigate) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H
import Kwap.App.Route as App.Route

data Query a = Navigate App.Route.Route a

sendNavigate
  :: ∀ o. App.Route.Route -> H.HalogenIO Query o Aff -> Aff (Maybe Unit)
sendNavigate route app = app.query $ H.mkTell $ Navigate route
