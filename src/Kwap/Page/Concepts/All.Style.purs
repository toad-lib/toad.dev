module Kwap.Page.Concepts.All.Style (container, link) where

import Kwap.Css
import Kwap.Css.Color
import Prelude

import CSS.Overflow (overflowY, scroll)

container :: CSS
container = do
  overflowY scroll
  display flex
  flexDirection column
  sym padding $ rem 2.0

link :: CSS
link = do
  backgroundColor $ Yellow Lightest
  sym padding $ rem 2.0
  marginBottom $ rem 2.0
  font $ fontFamily InterBold <> fontSize FontSizeH2
