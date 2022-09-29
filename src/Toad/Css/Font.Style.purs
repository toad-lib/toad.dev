module Toad.Css.Font.Style (typeRules) where

import Toad.Prelude

import CSS.Selector as Select
import Data.Foldable (traverse_)
import Toad.Css
  ( CSS
  , Selector
  , color
  , colorFg
  , definedIn
  , grey
  , margin
  , oklab
  , px
  , select
  , sym
  )

typeRules :: CSS
typeRules = do
  color <<< oklab <<< colorFg $ grey
  sym margin $ px 0.0
