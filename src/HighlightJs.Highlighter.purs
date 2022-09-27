module HighlightJs.Highlighter (HLJS, make, registerLanguage, highlight, RawHtml(..)) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import HighlightJs.Language (LanguageAlias(..), LanguageModule)

foreign import data HLJS :: Type

newtype RawHtml = RawHtml String

foreign import make :: Effect HLJS

foreign import registerLanguage_ ::
  {languageAliasString :: LanguageAlias -> String}
  -> LanguageAlias
  -> LanguageModule
  -> HLJS
  -> Effect Unit

registerLanguage ::
  LanguageAlias
  -> LanguageModule
  -> HLJS
  -> Effect Unit
registerLanguage = registerLanguage_ {languageAliasString: \(LanguageAlias s) -> s}

foreign import highlight_ :: { left :: ∀ l r. l -> Either l r
                             , right :: ∀ l r. r -> Either l r
                             , rawHtmlString :: RawHtml -> String
                             , languageAliasString :: LanguageAlias -> String
                             , rawHtml :: String -> RawHtml
                             }
                             -> LanguageAlias
                             -> RawHtml
                             -> HLJS
                             -> Effect (Either Error RawHtml)

highlight :: LanguageAlias
          -> RawHtml
          -> HLJS
          -> Effect (Either Error RawHtml)
highlight = highlight_ {left: Left, right: Right, rawHtmlString: \(RawHtml s) -> s, languageAliasString: \(LanguageAlias s) -> s, rawHtml: RawHtml}
