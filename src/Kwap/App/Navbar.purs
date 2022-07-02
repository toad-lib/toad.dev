module Kwap.App.Navbar (Section(..), render) where

import Data.Fist
import Prelude

import CSS.Common as Css.Common
import CSS.Size as Css.Size
import Data.Maybe (Maybe(..))
import Kwap.App.Atom.Logo as Atom.Logo
import Kwap.App.Css as Css
import Kwap.App.Css.Grid (GridCol(..), GridRow(..), grid, gridArea)
import Kwap.App.Html as HH
import Kwap.App.Layout (AppLayout(..))
import Kwap.App.Navbar.Button as Button

data Section = Home | Book | Concepts

derive instance eqSection :: Eq Section

data NavbarGridRegion
  = GridLogo
  | GridGapA
  | GridButtonA
  | GridButtonB
  | GridButtonC
  | GridRemainder

navbarGridRegionLabel :: NavbarGridRegion -> String
navbarGridRegionLabel = case _ of
  GridLogo -> "navbar-logo"
  GridGapA -> "navbar-gap-a"
  GridButtonA -> "navbar-button-a"
  GridButtonB -> "navbar-button-b"
  GridButtonC -> "navbar-button-c"
  GridRemainder -> "navbar-remainder"

navbarGrid :: AppLayout -> Css.CSS
navbarGrid _ = navbarGridDesktop

navbarGridDesktop :: Css.CSS
navbarGridDesktop =
  let
    pct = Css.pct >>> Css.anySize
    rem = Css.rem >>> Css.anySize
  in
    grid
      (fist1 (GridCol $ pct 100.0))
      [ GridRow (rem 8.0) (fist1 GridLogo)
      , GridRow (rem 2.0) (fist1 GridGapA)
      , GridRow (rem 6.0) (fist1 GridButtonA)
      , GridRow (rem 6.0) (fist1 GridButtonB)
      , GridRow (rem 6.0) (fist1 GridButtonC)
      , GridRow (pct 100.0) (fist1 GridRemainder)
      ]
      navbarGridRegionLabel

render :: ∀ a w. (Section -> a) -> AppLayout -> Section -> HH.HTML w a
render picked layout section =
  let
    isSelected test
      | test == section = Button.Selected
      | otherwise = Button.NotSelected
    select sec = picked sec
    inArea = gridArea navbarGridRegionLabel
    solidBg area' = HH.div
      [ Css.style do
          inArea area'
          Css.backgroundColor $ Css.Yellow Css.Lightest
      ]
      []
  in
    HH.div
      [ Css.style do
          navbarGrid layout
          Css.width $ Css.pct 100.0
          Css.height $ Css.pct 100.0
      ]
      [ Atom.Logo.render (Just $ inArea GridLogo)
      , Button.render (select Home) (isSelected Home) "home"
          (inArea GridButtonA)
      , Button.render (select Book) (isSelected Book) "book"
          (inArea GridButtonB)
      , Button.render (select Concepts) (isSelected Concepts) "concepts"
          (inArea GridButtonC)
      , solidBg GridGapA
      , solidBg GridRemainder
      ]
