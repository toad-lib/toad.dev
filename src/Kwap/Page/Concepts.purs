module Kwap.Page.Concepts (render) where

import Data.Maybe (Maybe)
import Kwap.Action as Action
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Page.Concepts.All as Self.All
import Kwap.Page.Concepts.One as Self.One
import Kwap.Route as Route

render
  :: ∀ w
   . Css.CSS
  -> Route.OneOrAll C.Path
  -> Maybe C.Manifest
  -> HH.HTML w Action.Action
render x (Route.One p) m = Self.One.render x p m
render x Route.All m = Self.All.render x m
