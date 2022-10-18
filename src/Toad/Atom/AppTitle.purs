module Toad.Atom.AppTitle where

import Toad.Prelude

import Data.Array (length)
import Data.Hashable (class Hashable)
import Toad.Atom.AppTitle.Style as Style
import Toad.Css as Css
import Toad.Html as Html

newtype AppTitle = AppTitle
  { hash :: Int, h1 :: Array Html.PlainHTML, accessory :: Array Html.PlainHTML }

derive instance ntAppTitle :: Newtype AppTitle _
instance showAppTitle :: Show AppTitle where
  show (AppTitle { hash, h1, accessory }) =
    fold
      [ "(AppTitle { h1: <"
      , show $ length h1
      , " elements>, accessory: "
      , show $ length accessory
      , " elements>, hash: "
      , show hash
      , "})"
      ]

instance eqAppTitle :: Eq AppTitle where
  eq (AppTitle { hash: a }) (AppTitle { hash: b }) = a == b

instance hashAppTitle :: Hashable AppTitle where
  hash (AppTitle { hash: h }) = h

render :: forall w i. Css.CSS -> AppTitle -> Html.HTML w i
render css (AppTitle { accessory, h1 }) =
  Html.div
    [ Css.style do
        Style.container
        css
    ]
    $ [ Html.h1 [ Style.h1Class ] (Html.fromPlainHTML <$> h1) ]
    <> (Html.fromPlainHTML <$> accessory)
