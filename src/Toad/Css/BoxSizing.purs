module Toad.Css.BoxSizing (global) where

import Toad.Prelude

import CSS.Selector as Select
import Toad.Css (CSS, borderBox, boxSizing, definedIn, select)

global :: CSS
global = select Select.star do
  definedIn "Toad.Css.BoxSizing"
  boxSizing borderBox
