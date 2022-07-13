module Kwap.Page.Concepts.One (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Route as Route

render :: ∀ w i. Css.CSS -> C.Path -> Maybe C.Manifest -> HH.HTML w i
render _ _ Nothing = HH.div_ []
render x p (Just m) = HH.div [ style x ] []
