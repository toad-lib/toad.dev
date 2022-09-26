module Toad.Style (appWrap, navbarWrap) where

import Prelude hiding (top)
import Toad.Css

import CSS.Overflow (overflow, overflowAuto)
import Toad.Css.Grid as Grid
import Toad.Layout (AppLayout(..))

appWrap :: CSS
appWrap = do
  position fixed
  width $ pct 100.0
  height $ pct 100.0
  top $ px 0.0
  left $ px 0.0
  Grid.appGrid AppLayoutDesktop
  backgroundColor <<< oklab <<< colorBg $ grey

navbarWrap :: CSS
navbarWrap = do
  Grid.inAppNavbar
  overflow overflowAuto
