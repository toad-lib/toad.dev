module Kwap.App.Page.Concepts.All (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as H
import Halogen.HTML.Properties as HP
import Kwap.App.Action as Action
import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Route as Route
import Kwap.Concept as C

renderOneLink :: forall w i. C.One -> HH.HTML w i
renderOneLink c = HH.a_
  (Route.Concepts <<< Route.One <<< Route.ConceptPath <<< C.path $ c)
  (C.title c)

render :: forall w i. Css.CSS -> Maybe C.Decl -> HH.HTML w i
render x Nothing = HH.div [ style x ] [ HH.h1_ `HH.withText` "Loading..." ]
render x (Just d) = HH.div [ style x ] $ renderOneLink <$> C.array d
