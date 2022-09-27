module HighlightJs (module X, highlight) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error)
import HighlightJs.Highlighter (RawHtml, make, registerLanguage)
import HighlightJs.Highlighter as H
import HighlightJs.Language (Language, defaultAlias, requireLanguage)

import HighlightJs.Highlighter (RawHtml(..)) as X
import HighlightJs.Language (Language(..)) as X

highlight :: Language -> RawHtml -> Effect (Either Error RawHtml)
highlight lang html = let
    hljs = do
      hljs' <- make
      registerLanguage (defaultAlias lang) (requireLanguage lang) hljs'
      pure hljs'
  in
    H.highlight (defaultAlias lang) html =<< hljs
