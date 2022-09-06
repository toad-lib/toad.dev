module Toad.Atom.Icon (Icon(..), render) where

import Toad.Prelude

import Halogen.HTML.Properties as HP
import Toad.Css (height, pct, style, width)
import Toad.Html as HH

foreign import _x :: String
foreign import _chevronUp :: String
foreign import _chevronDown :: String

data Icon = X | ChevronDown | ChevronUp

render :: ∀ w i. Icon -> HH.HTML w i
render i = HH.img
  [ style do
      height $ pct 100.0
      width $ pct 100.0
  , HP.src <<< url $ i
  ]

url :: Icon -> String
url X = _x
url ChevronDown = _chevronDown
url ChevronUp = _chevronUp
