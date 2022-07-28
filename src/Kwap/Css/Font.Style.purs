module Kwap.Css.Font.Style (global) where

import Kwap.Css
import Prelude

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
  definedIn "Kwap.Css.Font.Style"
  color Black
