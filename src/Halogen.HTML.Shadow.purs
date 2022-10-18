module Halogen.HTML.Shadow (shadow) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (ref)
import Halogen.HTML.Properties (IProp(..))
import Web.DOM (Element)

foreign import unsafeWrapInShadow :: Element -> Unit

shadow :: forall r i. IProp r i
shadow =
  let
    refHandler el = do
      el' <- el
      let _ = unsafeWrapInShadow el'
      Nothing
  in
    IProp $ ref refHandler
