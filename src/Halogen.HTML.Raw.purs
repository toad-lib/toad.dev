module Halogen.HTML.Raw (unsafeRawInnerHtml) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (ref)
import Halogen.HTML.Properties (IProp(..))
import Web.DOM (Element)

foreign import setInnerHtml :: Element -> String -> Unit

unsafeSetInnerHtml :: String -> Element -> Unit
unsafeSetInnerHtml html element = setInnerHtml element html

unsafeRawInnerHtml :: forall r i. String -> IProp r i
unsafeRawInnerHtml html =
  let
    refHandler el = do
      el' <- el
      let _ = unsafeSetInnerHtml html el'
      Nothing
  in
    IProp $ ref refHandler
