module Kwap.App.Navbar.Toast (module X, render) where

import Prelude

import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Icon as Icon
import Kwap.App.Navbar.Toast.Style (Status(..)) as X
import Kwap.App.Navbar.Toast.Style as Style

render :: forall w i. Css.CSS -> Style.Status -> String -> HH.HTML w i
render extra status text = HH.div
  [ Css.style do
      Style.container
      extra
      Style.statusStyle status
  ]
  [ HH.div [ Css.style Style.iconContainer ]
      [ Icon.render Icon.IconClose
      ]
  , HH.h4_ `HH.withText` text
  ]
