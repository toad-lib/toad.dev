module Toad.Markdown.Html.Style
  ( anchorSpan
  , documentBody
  , element
  , elementFirst
  , span
  , topLevel
  ) where

import Toad.Css
import Toad.Prelude

import Data.Color.OkLab (Lightness(..))
import Toad.Html as Html

documentBody :: CSS
documentBody = do
  sym padding $ rem 2.0
  backgroundColor ∘ oklab ∘ green $ Lightness 0.96

element :: CSS
element = do
  marginTop $ rem 1.0

elementFirst :: CSS
elementFirst = do
  element
  marginTop nil

span :: CSS
span = do
  font mempty
  color ∘ oklab ∘ green $ Lightness 0.10

topLevel :: CSS
topLevel = display block

anchorSpan :: CSS
anchorSpan = do
  font $ fontFamily AtkinsonBold
  let color' = oklab ∘ green $ Lightness 0.20
  color color'
  key (fromString "text-decoration")
    (noCommas [ value "wavy underline ", value color' ])
