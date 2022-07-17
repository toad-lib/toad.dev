module Kwap.Page.Concepts.One (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML.CSS (style)
import Kwap.Concept as C
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Markdown as Md
import Kwap.Markdown.Html as Md.Html
import Kwap.Route as Route

render :: ∀ w i. Css.CSS -> Maybe Md.Document -> HH.HTML w i
render _ Nothing = HH.div_ []
render x (Just md) = HH.div [ style x ] [ Md.Html.render md ]
