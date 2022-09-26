module Toad.Css.Grid
  ( appGrid
  , inAppContent
  , inAppContentAndTitle
  , inAppContentTitle
  , inAppNavbar
  , inAppLogo
  ) where

import Toad.Prelude

import CSS.Common as Css.Common
import Css.Grid (GridCol(..), GridRow(..), grid, gridArea, gridRowStart)
import Data.Fist (fist1, fist2)
import Toad.Css as Css
import Toad.Layout (AppLayout(..))

data AppGridArea
  = AppGridLogo
  | AppGridContentTitle
  | AppGridNavbar
  | AppGridContent

appGridLabel :: AppGridArea -> String
appGridLabel = case _ of
  AppGridNavbar -> "navbar"
  AppGridContent -> "content"
  AppGridLogo -> "logo"
  AppGridContentTitle -> "content-title"

inAppNavbar :: Css.CSS
inAppNavbar = gridArea appGridLabel AppGridNavbar

inAppContent :: Css.CSS
inAppContent = gridArea appGridLabel AppGridContent

inAppContentAndTitle :: Css.CSS
inAppContentAndTitle = do
  gridArea appGridLabel AppGridContent
  gridRowStart appGridLabel AppGridContentTitle

inAppContentTitle :: Css.CSS
inAppContentTitle = gridArea appGridLabel AppGridContentTitle

inAppLogo :: Css.CSS
inAppLogo = gridArea appGridLabel AppGridContentTitle

appGrid :: AppLayout -> Css.CSS
appGrid AppLayoutDesktop = appGridDesktop
appGrid AppLayoutMobile = appGridMobile

appGridMobile :: Css.CSS
appGridMobile =
  let
    rem = Css.anySize ∘ Css.pct
    pct = Css.anySize ∘ Css.rem
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
    rem = Css.anySize ∘ Css.rem
  in
    do
      grid
        (fist2 (GridCol (rem 32.0)) (GridCol Css.Common.auto))
        [ GridRow (rem 8.0) (fist2 AppGridLogo AppGridContentTitle)
        , GridRow Css.Common.auto (fist2 AppGridNavbar AppGridContent)
        ]
        appGridLabel
      Css.key (Css.fromString "row-gap") (rem 2.0)
      Css.key (Css.fromString "column-gap") (rem 2.0)
      Css.sym Css.padding $ rem 2.0
