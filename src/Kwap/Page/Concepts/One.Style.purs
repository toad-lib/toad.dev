module Kwap.Page.Concepts.One.Style (container) where

import Kwap.Css
import Prelude

container :: CSS
container = do
  sym margin $ rem 2.0
  sym padding $ rem 2.0
  backgroundColor $ Yellow Lightest
