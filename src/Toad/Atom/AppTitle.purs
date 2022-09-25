module Toad.Atom.AppTitle where

import Toad.Prelude

import Data.Array (length)
import Toad.Atom.AppTitle.Style as Style
import Toad.Css as Css
import Toad.Html as Html
import Unsafe.Reference (UnsafeRefEq(..))

newtype AppTitle = AppTitle (Array Html.PlainHTML)

derive instance ntAppTitle :: Newtype AppTitle _
instance showAppTitle :: Show AppTitle where
  show (AppTitle a) = "(AppTitle <" <> (show $ length a) <> " plain elements>)"

instance eqAppTitle :: Eq AppTitle where
  eq a b = (UnsafeRefEq a) == (UnsafeRefEq b)

render :: forall w i. Css.CSS -> AppTitle -> Html.HTML w i
render css (AppTitle title) =
  Html.div
    [ Css.style do
        Style.container
        css
    ]
    [ Html.h1 [ Style.h1Class ] (Html.fromPlainHTML <$> title)
    ]
