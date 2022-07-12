module Kwap.Page.Concepts (render) where

import Data.Maybe (Maybe)
import Kwap.Action as Action
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Page.Concepts.All as Self.All
import Kwap.Route as Route

render
  :: forall w
   . Route.OneOrAll Route.ConceptPath
  -> Css.CSS
  -> Maybe C.Decl
  -> HH.HTML w Action.Action
render (Route.One _) _ _ = HH.h1_ [ HH.text "TODO" ]
render Route.All c d = Self.All.render c d
