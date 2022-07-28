module Toad.Page.Concepts.All (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as H
import Halogen.HTML.Properties as HP
import Toad.Concept as C
import Toad.Css as Css
import Toad.Html as HH
import Toad.Page.Concepts.All.Style as Style
import Toad.Route as Route

conceptLink :: ∀ w i. C.Decl -> HH.HTML w i
conceptLink c = HH.a
  [ style Style.link ]
  (Route.Concepts <<< Route.One <<< C.ident $ c)
  (pure <<< HH.text <<< C.titleString <<< C.title $ c)

render :: ∀ w i. Css.CSS -> Maybe C.Manifest -> HH.HTML w i
render x Nothing = HH.div [ style x ] [ HH.h3_ `HH.withText` "Loading..." ]
render x (Just m) =
  HH.div
    [ style do
        Style.container
        x
    ] $ conceptLink <$> C.decls m
