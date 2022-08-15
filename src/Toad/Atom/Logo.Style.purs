module Toad.Atom.Logo.Style where

import Toad.Prelude hiding (top)

import CSS.Common as Css.Common
import Toad.Css (CSS, display, flex, justifyContent)

foreign import toadLogoUrl :: String

logoContainer :: CSS
logoContainer = do
  display flex
  justifyContent Css.Common.center
