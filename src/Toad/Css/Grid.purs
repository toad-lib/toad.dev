module Toad.Css.Grid
  ( appGrid
  , inAppContent
  , inAppNavbar
  ) where

import Toad.Prelude

import CSS.Common as Css.Common
import Css.Grid (GridCol(..), GridRow(..), grid, gridArea)
import Data.Fist (fist1, fist2)
import Toad.Css as Css
import Toad.Layout (AppLayout(..))

data AppGridArea
  = AppGridNavbar
  | AppGridContent

appGridLabel :: AppGridArea -> String
appGridLabel = case _ of
  AppGridNavbar -> "navbar"
  AppGridContent -> "content"

inAppNavbar :: Css.CSS
inAppNavbar = gridArea appGridLabel AppGridNavbar

inAppContent :: Css.CSS
inAppContent = gridArea appGridLabel AppGridContent

appGrid :: AppLayout -> Css.CSS
appGrid AppLayoutDesktop = appGridDesktop
appGrid AppLayoutMobile = appGridMobile

appGridMobile :: Css.CSS
appGridMobile =
  let
    rem = Css.rem >>> Css.anySize
    pct = Css.pct >>> Css.anySize
  in
    grid
      (fist1 $ GridCol (pct 100.0))
      [ GridRow (rem 16.0) $ fist1 AppGridNavbar
      , GridRow Css.Common.auto $ fist1 AppGridContent
      ]
      appGridLabel

appGridDesktop :: Css.CSS
appGridDesktop =
  let
    rem = Css.rem >>> Css.anySize
    pct = Css.pct >>> Css.anySize
  in
    grid
      (fist2 (GridCol (rem 16.0)) (GridCol Css.Common.auto))
      [ GridRow (pct 100.0) (fist2 AppGridNavbar AppGridContent)
      ]
      appGridLabel
