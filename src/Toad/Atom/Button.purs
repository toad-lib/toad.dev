module Toad.Atom.Button (module X, render, renderPlain) where

import Toad.Prelude

import Data.Active (Active(..))
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Shadow (shadow)
import Toad.Atom.Button.Style (Theme(..), primary) as X
import Toad.Atom.Button.Style as Style
import Toad.Css (CSS, style)
import Toad.Html as Html

render
  :: forall w i
   . { active :: Active
     , children :: Array (Html.HTML w i)
     , onClick :: i
     , styleContainer :: CSS
     , styleButton :: CSS
     , theme :: Style.Theme
     }
  -> Html.HTML w i
render
  { active
  , children
  , onClick: onClickAction
  , styleButton
  , styleContainer
  , theme
  } =
  Html.div
    [ shadow, style styleContainer ]
    [ stylesheet (Style.button theme)
    , Html.button
        [ Html.classNames
            $ [ Style.className ]
            <> case active of
              Active -> [ Style.activeClassName ]
              Inactive -> []
        , onClick $ const onClickAction
        , style styleButton
        ]
        children
    ]

renderPlain
  :: { children :: Array Html.PlainHTML
     , styleContainer :: CSS
     , styleButton :: CSS
     , theme :: Style.Theme
     }
  -> Html.PlainHTML
renderPlain { styleContainer, styleButton, children, theme } =
  Html.div
    [ shadow, style styleContainer ]
    [ stylesheet (Style.button theme)
    , Html.button
        [ Html.classNames [ Style.className ]
        , style styleButton
        ]
        children
    ]
