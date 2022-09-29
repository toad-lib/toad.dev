module Toad.Markdown.Html where

import Toad.Prelude

import CSS as Css.Core
import Data.Array (drop, take)
import Data.Array.NonEmpty as NEArray
import Data.String.NonEmpty as NEString
import Data.Either (either, hush)
import Data.Filterable (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (hash)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Raw (unsafeRawInnerHtml)
import HighlightJs as Highlight
import Toad.Css (CSS, pt)
import Toad.Html as Html
import Toad.Markdown (Anchor(..), CodeFence(..), CodeFenceFileType(..), Document, Element(..), Heading(..), Span(..), Text(..), Token(..), elements, spanString)
import Toad.Markdown.Html.Style as Style
import Toad.Route as Route

hashSpan :: Span -> Int
hashSpan s = hash ∘ spanString $ s

renderText :: Text -> Html.PlainHTML
renderText (Unstyled s) = Html.text s
renderText (Bold s) = Html.b_ [ renderText ∘ Unstyled $ s ]
renderText (Italic s) = Html.i_ [ renderText ∘ Unstyled $ s ]
renderText (BoldItalic s) = Html.b_ [ renderText ∘ Italic $ s ]
renderText (InlineCode s) = Html.code_ [ renderText ∘ Unstyled $ s ]

renderAnchor :: Anchor -> Html.PlainHTML
renderAnchor (Anchor s u) =
  HH.a
    [ HP.href u ]
    [ Html.span [ style Style.anchorSpan ]
        ∘ NEArray.toArray
        ∘ map renderText
        $ s
    ]
renderAnchor (ConceptAnchor s i) =
  Html.a_
    (Route.Concepts $ Route.One i)
    [ Html.span [ style Style.anchorSpan ]
        ∘ NEArray.toArray
        ∘ map renderText
        $ s
    ]

renderToken :: Token -> Html.PlainHTML
renderToken (TextToken t) = renderText t
renderToken (AnchorToken a) = renderAnchor a

renderSpan :: CSS -> Span -> Html.PlainHTML
renderSpan s (Span ts) = Html.span
  [ style s ]
  (NEArray.toArray ∘ map renderToken $ ts)

renderHeading :: CSS -> Heading -> Html.PlainHTML
renderHeading x =
  let
    style' f = style do
      x
      Html.headingStyle f
    span' = renderSpan $ pure unit
  in
    case _ of
      H1 s -> Html.h1 [ style' Html.h1Font ] [ span' s ]
      H2 s -> Html.h2 [ style' Html.h2Font ] [ span' s ]
      H3 s -> Html.h3 [ style' Html.h3Font ] [ span' s ]
      H4 s -> Html.h4 [ style' Html.h4Font ] [ span' s ]
      H5 s -> Html.h5 [ style' Html.h5Font ] [ span' s ]
      H6 s -> Html.h6 [ style' Html.h6Font ] [ span' s ]

renderCodeFence :: CSS -> CodeFence -> Effect Html.PlainHTML
renderCodeFence css (CodeFence lang code) = do
  let lang' = case (\(CodeFenceFileType s) -> NEString.toString s) <$> lang of
                Just "javascript" -> Highlight.Javascript
                Just "rust" -> Highlight.Rust
                _ -> Highlight.Plaintext

  result <- Highlight.highlight lang' (Highlight.RawHtml code)
  let rawHtml = either (const "failed") (\(Highlight.RawHtml r) -> r) result
  pure
    $ HH.pre
        [ style do
            css
            Css.Core.fontSize $ pt 16.0
        ]
        [ HH.code [ Html.classNames ["hljs"], unsafeRawInnerHtml rawHtml ] [] ]

shouldSkip :: Element -> Boolean
shouldSkip (ElementComment _) = true
shouldSkip (ElementList _) = true
shouldSkip _ = false

isComment :: Element -> Boolean
isComment (ElementComment _) = true
isComment _ = false

renderHeaderSpan
  :: Document -> Maybe ({ elems :: Array Html.PlainHTML, hash :: Int })
renderHeaderSpan doc =
  case take 1 ∘ filter (not isComment) ∘ elements $ doc of
    [ ElementHeading (H1 span) ] ->
      Just
        { elems: [ renderSpan (pure unit) span ]
        , hash: hashSpan span
        }
    -- this should be unreachable
    -- TODO(orion): change the interface of Document to include
    -- a type-safe H1 (e.g. Document {heading :: Heading, body :: Array Element})
    _ -> Nothing

renderBody :: CSS -> Document -> Effect Html.PlainHTML
renderBody x d =
  let
    style' 0 = Style.elementFirst
    style' _ = Style.element

    render' :: Int -> Element -> Effect Html.PlainHTML
    render' _ (ElementComment _) = pure $ HH.div_ []
    render' i (ElementCodeFence cf) =
      renderCodeFence
        ( do
            Style.topLevel
            style' i
        )
        cf
    render' _ (ElementList _) = pure $ HH.div_ []
    render' i (ElementHeading h) =
      pure $ renderHeading
        ( do
            Style.topLevel
            style' i
        )
        h
    render' i (ElementSpan s) =
      pure $ renderSpan
        ( do
            Style.span
            Style.topLevel
            style' i
        )
        s
  in
    do
      xs <- sequence
        ∘ mapWithIndex render'
        ∘ filter (not shouldSkip)
        ∘ drop 1
        ∘ filter (not isComment)
        ∘ elements
        $ d

      pure $
        Html.div
          [ style do
              Style.documentBody
              x
          ]
          xs
