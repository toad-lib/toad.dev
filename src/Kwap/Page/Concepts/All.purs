module Kwap.Page.Concepts.All (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as H
import Halogen.HTML.Properties as HP
import Kwap.Action as Action
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Route as Route

renderOneLink :: forall w i. C.One -> HH.HTML w i
renderOneLink c = HH.a_
  (Route.Concepts <<< Route.One <<< Route.ConceptPath <<< C.path $ c)
  (C.title c)

render :: forall w i. Css.CSS -> Maybe C.Decl -> HH.HTML w i
render x Nothing = HH.div [ style x ] [ HH.h1_ `HH.withText` "Loading..." ]
render x (Just d) = HH.div [ style x ] $ renderOneLink <$> C.array d
