module Toad.Query (Query(..), sendNavigate) where

import Toad.Prelude

import Effect.Aff (Aff)
import Halogen as H
import Toad.Route as Route

data Query a = Navigate Route.Route a

sendNavigate
  :: ∀ o. Route.Route -> H.HalogenIO Query o Aff -> Aff (Maybe Unit)
sendNavigate route app = app.query $ H.mkTell $ Navigate route
