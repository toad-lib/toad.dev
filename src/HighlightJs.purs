module HighlightJs (module X, highlight) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error)
import HighlightJs.Highlighter (RawHtml(..)) as X
import HighlightJs.Highlighter as Highlighter
import HighlightJs.Language (Language(..)) as X
import HighlightJs.Language as Lang

highlight
  :: Lang.Language
  -> Highlighter.RawHtml
  -> Effect (Either Error Highlighter.RawHtml)
highlight lang html =
  let
    hljs = do
      hljs' <- Highlighter.make
      Highlighter.registerLanguage (Lang.defaultAlias lang)
        (Lang.requireLanguage lang)
        hljs'
      pure hljs'
  in
    Highlighter.highlight (Lang.defaultAlias lang) html =<< hljs
