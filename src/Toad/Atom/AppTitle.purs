module Toad.Atom.AppTitle where

import Toad.Prelude

import Data.Array (length)
import Data.Hashable (class Hashable)
import Toad.Atom.AppTitle.Style as Style
import Toad.Css as Css
import Toad.Html as Html

newtype AppTitle = AppTitle { hash :: Int, elems :: Array Html.PlainHTML }

derive instance ntAppTitle :: Newtype AppTitle _
instance showAppTitle :: Show AppTitle where
  show (AppTitle { hash, elems }) =
    fold
      [ "(AppTitle { elems: <"
      , show $ length elems
      , " elements>, hash: "
      , show hash
      , ")"
      ]

instance eqAppTitle :: Eq AppTitle where
  eq (AppTitle { hash: a }) (AppTitle { hash: b }) = a == b

instance hashAppTitle :: Hashable AppTitle where
  hash (AppTitle { hash: h }) = h

render :: forall w i. Css.CSS -> AppTitle -> Html.HTML w i
render css (AppTitle { elems }) =
  Html.div
    [ Css.style do
        Style.container
        css
    ]
    [ Html.h1 [ Style.h1Class ] (Html.fromPlainHTML <$> elems)
    ]
