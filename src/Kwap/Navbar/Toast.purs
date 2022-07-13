module Kwap.Navbar.Toast (module X, render) where

import Prelude

import Halogen.HTML.Events as HE
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Icon as Icon
import Kwap.Navbar.Toast.Style (Status(..)) as X
import Kwap.Navbar.Toast.Style as Style

render :: forall w i. Css.CSS -> i -> Style.Status -> String -> HH.HTML w i
render x i s t = HH.div
  [ Css.style do
      Style.container
      x
      Style.statusStyle s
  ]
  [ HH.div
      [ Css.style Style.iconContainer, HE.onClick $ const i ]
      [ Icon.render Icon.IconClose
      ]
  , HH.h4_ `HH.withText` t
  ]
