module Toad.Markdown.Html.Style
  ( anchorSpan
  , documentBody
  , githubButton
  , githubButtonTheme
  , span
  , topLevel
  , topLevelFirst
  , topLevelSpan
  ) where

import Toad.Css
import Toad.Prelude

import CSS.Overflow (overflow, overflowAuto)
import Data.Color.OkLab (Lightness(..))
import Toad.Atom.AppTitle.Style as AppTitle.Style
import Toad.Atom.Button as Button

documentBody :: CSS
documentBody = do
  height $ pct 100.0
  overflow overflowAuto
  sym padding $ rem 2.0
  backgroundColor ∘ oklab ∘ green $ Lightness 0.96

span :: CSS
span = do
  font mempty
  color ∘ oklab ∘ green $ Lightness 0.10

topLevel :: CSS
topLevel = do
  display block
  marginTop $ rem 2.0

topLevelFirst :: CSS
topLevelFirst = do
  topLevel
  marginTop nil

topLevelSpan :: CSS
topLevelSpan = do
  span
  display block
  marginTop $ rem 1.0

anchorSpan :: CSS
anchorSpan = do
  font $ fontFamily AtkinsonBold
  let color' = oklab ∘ green $ Lightness 0.20
  color color'
  key (fromString "text-decoration")
    (noCommas [ value "wavy underline ", value color' ])

githubButton :: CSS
githubButton = do
  sym padding $ rem 1.0
  height $ rem 3.0

githubButtonTheme :: Button.Theme
githubButtonTheme = Button.Theme
  { color: green
  , inert: AppTitle.Style.backgroundLightness
  , hover: Lightness 0.85
  , active: Lightness 0.90
  }
