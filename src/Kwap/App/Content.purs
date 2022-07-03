module Kwap.App.Content (render) where

import Prelude

import Kwap.App.Css as Css
import Kwap.App.Html as HH

render :: forall w i. HH.HTML w i
render = HH.div
  [ Css.style do
      Css.backgroundColor $ Css.Yellow Css.Lightest
  ]
  []
