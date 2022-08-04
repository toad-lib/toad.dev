module Toad.Page.Concepts.One (render) where

import Toad.Prelude

import Halogen.HTML.CSS (style)
import Toad.Css as Css
import Toad.Html as HH
import Toad.Markdown as Md
import Toad.Markdown.Html as Md.Html
import Toad.Page.Concepts.One.Style as Style

render :: ∀ w i. Css.CSS -> Maybe Md.Document -> HH.HTML w i
render _ Nothing = HH.div_ []
render x (Just md) = HH.div
  [ style do
      Style.container
      x
  ]
  [ Md.Html.render md ]
