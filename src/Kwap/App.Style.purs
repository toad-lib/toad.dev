module Kwap.App.Style (appBackground, appWrap, contentWrap, navbarWrap) where

import Kwap.App.Css
import Prelude hiding (top)

import Kwap.App.Css.Grid as Grid
import Kwap.App.Layout (AppLayout(..))

appBackground :: KwapGradient -> CSS
appBackground grabent = do
  width $ pct 100.0
  height $ pct 100.0
  position fixed
  top $ px 0.0
  left $ px 0.0
  zIndex $ -1000
  kwapGradient grabent

appWrap :: CSS
appWrap = do
  position fixed
  width $ pct 100.0
  height $ pct 100.0
  top $ px 0.0
  left $ px 0.0
  Grid.appGrid AppLayoutDesktop

contentWrap :: CSS
contentWrap = do
  Grid.inAppContent

navbarWrap :: CSS
navbarWrap = do
  border solid (rem 2.0) (cssColor $ Yellow Lightest)
  Grid.inAppNavbar
