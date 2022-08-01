module Toad.Css.Font.Style (global) where

import Prelude
import Toad.Css

import CSS.Selector as Sel
import Data.Foldable (traverse_)

global :: CSS
global =
  traverse_
    (flip select typeRules <<< Sel.element)
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
  color Black
