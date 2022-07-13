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

conceptLink :: ∀ w i. C.Decl -> HH.HTML w i
conceptLink c = HH.a_
  (Route.Concepts <<< Route.One <<< C.path $ c)
  (C.titleString <<< C.title $ c)

render :: ∀ w i. Css.CSS -> Maybe C.Manifest -> HH.HTML w i
render x Nothing = HH.div [ style x ] [ HH.h1_ `HH.withText` "Loading..." ]
render x (Just m) = HH.div [ style x ] $ conceptLink <$> C.decls m
