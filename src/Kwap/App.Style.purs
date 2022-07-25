module Kwap.Style (appWrap, navbarWrap) where

import Kwap.Css
import Prelude hiding (top)

import Kwap.Css.Grid as Grid
import Kwap.Layout (AppLayout(..))

appWrap :: CSS
appWrap = do
  position fixed
  width $ pct 100.0
  height $ pct 100.0
  top $ px 0.0
  left $ px 0.0
  Grid.appGrid AppLayoutDesktop

navbarWrap :: CSS
navbarWrap = do
  Grid.inAppNavbar
