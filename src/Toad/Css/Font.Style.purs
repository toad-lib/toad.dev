module Toad.Css.Font.Style (global) where

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

applyStyle :: CSS -> Selector -> CSS
applyStyle = flip select

global :: CSS
global =
  traverse_
    (applyStyle typeRules)
    <<< map Select.element
    $
      [ "p"
      , "span"
      , "h1"
      , "h2"
      , "h3"
      , "h4"
      , "h5"
      , "h6"
      , "a"
      ]

typeRules :: CSS
typeRules = do
  definedIn "Toad.Css.Font.Style"
  color <<< oklab <<< colorFg $ grey
  sym margin $ px 0.0
