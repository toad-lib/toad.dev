module Toad.Markdown.Html.Style
  ( anchorSpan
  , document
  , documentBody
  , element
  , elementFirst
  , headerContainer
  , headerClass
  , span
  , topLevel
  , global
  ) where

import Toad.Css
import Toad.Prelude

import CSS.Selector as Select
import Data.Color.OkLab (Lightness(..))
import Toad.Html as Html

global :: CSS
global = do
  select
    ( Select.element "h1"
        `Select.with` Select.byClass headerClassString
    )
    (Html.headingStyle Html.h1Font)

  -- h1.md-header *
  select
    ( Select.element "h1"
        `Select.with` Select.byClass headerClassString
        `Select.deep` Select.star
    )
    (color ∘ oklab ∘ green $ Lightness 0.05)

headerClassString :: String
headerClassString = "md-header"

headerClass :: forall i r. Html.ClassProp i r
headerClass = Html.classNames [ headerClassString ]

document :: CSS
document = do
  display flex
  flexDirection column

documentBody :: CSS
documentBody = do
  sym padding $ rem 1.0
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
  key (fromString "text-decoration") (noCommas [value "wavy underline ", value color'])

headerContainer :: CSS
headerContainer = do
  backgroundColor ∘ oklab ∘ green $ Lightness 0.75
  sym padding $ rem 1.5
  paddingLeft $ rem 2.0
