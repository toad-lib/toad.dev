module Kwap.App.Page.Concepts (render) where

import Data.Maybe (Maybe)
import Kwap.App.Action as Action
import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Page.Concepts.All as Self.All
import Kwap.App.Route as Route
import Kwap.Concept as C

render
  :: forall w
   . Route.OneOrAll Route.ConceptPath
  -> Css.CSS
  -> Maybe C.Decl
  -> HH.HTML w Action.Action
render (Route.One _) _ _ = HH.h1_ [ HH.text "TODO" ]
render Route.All c d = Self.All.render c d
