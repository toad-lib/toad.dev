module Kwap.Navbar.Toast.Style
  ( Status(..)
  , container
  , statusStyle
  , iconContainer
  ) where

import CSS.Cursor as Css.Cursor
import Kwap.Css
  ( CSS
  , Color(..)
  , Shade(..)
  , absolute
  , backgroundColor
  , color
  , cursor
  , height
  , padding
  , paddingTop
  , position
  , relative
  , rem
  , right
  , sym
  , top
  , width
  )
import Prelude (discard, ($))

data Status = StatusError | StatusWarn | StatusInfo

container :: CSS
container = do
  position relative
  sym padding $ rem 1.0
  paddingTop $ rem 2.0

iconContainer :: CSS
iconContainer = do
  position absolute
  height $ rem 2.0
  width $ rem 2.0
  top $ rem 0.25
  right $ rem 0.25
  cursor Css.Cursor.Pointer

statusStyle :: Status -> CSS
statusStyle StatusError = do
  backgroundColor $ Red Light
  color $ Red Dark
statusStyle StatusWarn = do
  backgroundColor $ Yellow Light
  color $ Yellow Dark
statusStyle StatusInfo = do
  backgroundColor $ Purple Light
  color $ Purple Dark
