module Kwap.App.Atom.Logo.Style
  ( kwapMaskUrl
  , logoContainer
  ) where

import Kwap.App.Css
import Prelude hiding (top)

import CSS.Common as Css.Common
import CSS.Size as Css.Size
import Data.Maybe (Maybe(..), maybe)

foreign import kwapMaskUrl :: String

logoContainer :: CSS
logoContainer = do
  position relative
  Css.Size.sym padding $ rem 2.0
  display flex
  justifyContent Css.Common.center
