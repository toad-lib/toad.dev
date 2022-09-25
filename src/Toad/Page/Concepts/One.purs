module Toad.Page.Concepts.One (render) where

import Toad.Prelude

import Halogen.HTML.CSS (style)
import Toad.Css as Css
import Toad.Html as Html
import Toad.Markdown as Md
import Toad.Markdown.Html as Md.Html

render :: ∀ w i. Css.CSS -> Maybe Md.Document -> Html.HTML w i
render x d = maybe (Html.div [] []) (Md.Html.renderBody (Just x)) d
