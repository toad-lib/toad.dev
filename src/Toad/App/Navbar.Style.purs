module Toad.App.Navbar.Style where

import Toad.Prelude hiding (bottom)

import CSS (marginLeft, paddingLeft)
import CSS.Common (hidden)
import CSS.Display (displayNone)
import Data.Int as Int
import Data.Newtype (class Newtype, unwrap)
import Toad.App.Navbar.Internal (Active(..), ChildIs(..), SiblingIs(..), Visible(..))
import Toad.Css (StyleM, absolute, backgroundColor, borderBox, bottom, boxSizing, color, colorBg, colorFg, colorPrimary, colorPrimary2, display, flexGrow, green, grey, height, left, oklab, padding, paddingBottom, paddingTop, pct, position, relative, rem, sym, visibility, width)

navbar :: StyleM Unit
navbar = do
  backgroundColor <<< oklab <<< colorFg $ grey
  height $ pct 100.0
  position relative

navbarWrapper :: StyleM Unit
navbarWrapper = do
  boxSizing borderBox
  sym padding $ rem 1.0
  height $ pct 100.0

itemText :: Active -> StyleM Unit
itemText a =
  case a of
    Active -> color <<< oklab <<< colorFg $ green
    _ -> color <<< oklab <<< colorBg $ green

itemRibbon :: SiblingIs Active -> ChildIs Active -> StyleM Unit
itemRibbon sa ca = do
  width (rem 1.0)
  backgroundColor
    <<< oklab
    <<< colorPrimary2
    $ green
  case ca, sa of
    ChildIs Inactive, SiblingIs Inactive -> visibility hidden
    _, _ -> pure unit

itemUnderline :: StyleM Unit
itemUnderline = do
  position absolute
  bottom $ rem 0.0
  left $ rem 0.0
  width $ pct 100.0
  height $ rem 0.125
  backgroundColor <<< oklab <<< colorPrimary $ green

itemWrapper :: forall d. Newtype d Int => d -> Visible -> Active -> StyleM Unit
itemWrapper d e a = do
  position relative
  flexGrow 1.0
  paddingTop $ rem 1.0
  paddingBottom $ rem 1.0
  paddingLeft $ rem 1.0
  marginLeft
    <<< rem
    <<< Int.toNumber
    <<< (_ + 1)
    <<< (_ * 2)
    <<< unwrap
    $ d

  case e of
    Hidden -> display displayNone
    _ -> pure unit

  case a of
    Active ->
      do
        backgroundColor
        <<< oklab
        <<< colorPrimary2
        $ green
    _ -> pure unit
