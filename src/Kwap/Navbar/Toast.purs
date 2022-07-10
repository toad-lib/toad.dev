module Kwap.Navbar.Toast (module X, render) where

import Prelude

import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Icon as Icon
import Kwap.Navbar.Toast.Style (Status(..)) as X
import Kwap.Navbar.Toast.Style as Style

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
