module Kwap.Page.Concepts.One (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Markdown as Md
import Kwap.Markdown.Html as Md.Html
import Kwap.Page.Concepts.One.Style as Style

render :: ∀ w i. Css.CSS -> Maybe Md.Document -> HH.HTML w i
render _ Nothing = HH.div_ []
render x (Just md) = HH.div
  [ style do
      Style.container
      x
  ]
  [ Md.Html.render md ]
