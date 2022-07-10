module Kwap.Query (Query(..), sendNavigate) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H
import Kwap.Route as Route

data Query a = Navigate Route.Route a

sendNavigate
  :: ∀ o. Route.Route -> H.HalogenIO Query o Aff -> Aff (Maybe Unit)
sendNavigate route app = app.query $ H.mkTell $ Navigate route
