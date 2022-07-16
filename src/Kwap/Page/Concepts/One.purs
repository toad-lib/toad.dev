module Kwap.Page.Concepts.One (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML.CSS (style)
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Markdown as Md
import Kwap.Route as Route

render :: ∀ w i. Css.CSS -> Maybe (Tuple C.Decl Md.Document) -> HH.HTML w i
render _ Nothing = HH.div_ []
render x (Just _) = HH.div [ style x ] []
