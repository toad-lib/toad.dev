module Kwap.App.Icon (Icon(..), render) where

import Prelude

import Halogen.HTML.Properties as HP
import Kwap.App.Css as Css
import Kwap.App.Html as HH

foreign import iconCloseUrl :: String

data Icon = IconClose

render :: ∀ w i. Icon -> HH.HTML w i
render i = HH.img
  [ Css.style do
      Css.height $ Css.pct 100.0
      Css.width $ Css.pct 100.0
  , HH.classNames [ "icon" ]
  , HP.src $ url i
  ]

url :: Icon -> String
url IconClose = iconCloseUrl
