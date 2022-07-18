module Kwap.Markdown.Html.Style (document, element, elementFirst, span) where

import Kwap.Css
import Prelude

document :: CSS
document = do
  display flex
  flexDirection column

element :: CSS
element = do
  marginTop $ rem 1.0

elementFirst :: CSS
elementFirst = do
  element
  marginTop nil

span :: CSS
span = font mempty
