module Toad.Markdown.Html where

import Toad.Prelude

import CSS as Css.Core
import Data.Active (Active(..))
import Data.Array (drop, take)
import Data.Array.NonEmpty as NEArray
import Data.Either (either, hush)
import Data.Filterable (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (hash)
import Data.Newtype (unwrap)
import Data.String.NonEmpty as NEString
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (href, rel, target)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Raw (unsafeRawInnerHtml)
import HighlightJs as Highlight
import Toad.Atom.AppTitle (AppTitle(..))
import Toad.Atom.Button as Button
import Toad.Atom.Icon as Icon
import Toad.Concept (Path(..))
import Toad.Concept.Fetch as Concept.Remote
import Toad.Css (CSS, green, marginBottom, nil, pt)
import Toad.Html as Html
import Toad.Markdown
  ( Anchor(..)
  , CodeFence(..)
  , CodeFenceFileType(..)
  , Document
  , Element(..)
  , Heading(..)
  , List(..)
  , ListToken(..)
  , Span(..)
  , Text(..)
  , Token(..)
  , elements
  , spanString
  )
import Toad.Markdown.Html.Style as Style
import Toad.Route as Route
import Unsafe.Coerce (unsafeCoerce)

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
      Html.headingStyle f
      x
    span' = renderSpan $ pure unit
  in
    case _ of
      H1 s -> Html.h1 [ style' Html.h1Font ] [ span' s ]
      H2 s -> Html.h2 [ style' Html.h2Font ] [ span' s ]
      H3 s -> Html.h3 [ style' Html.h3Font ] [ span' s ]
      H4 s -> Html.h4 [ style' Html.h4Font ] [ span' s ]
      H5 s -> Html.h5 [ style' Html.h5Font ] [ span' s ]
      H6 s -> Html.h6 [ style' Html.h6Font ] [ span' s ]

renderCode :: CSS -> CodeFence -> Effect Html.PlainHTML
renderCode css (CodeFence lang code) = do
  let
    lang' = case (\(CodeFenceFileType s) -> NEString.toString s) <$> lang of
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
            marginBottom nil
        ]
        [ HH.code [ Html.classNames [ "hljs" ], unsafeRawInnerHtml rawHtml ] []
        ]

renderListToken :: ListToken -> Html.PlainHTML
renderListToken (ListTokenSpan s) =
  Html.li
    [ style Style.span ]
    [ renderSpan (pure unit) s ]
renderListToken (ListTokenSpanSublist s l) =
  Html.li
    [ style Style.span ]
    [ Html.div
        []
        [ renderSpan (pure unit) s
        , renderList (pure unit) l
        ]
    ]

renderListUsing
  :: (Array Html.PlainHTML -> Html.PlainHTML)
  -> NEArray.NonEmptyArray ListToken
  -> Html.PlainHTML
renderListUsing parent tokens = parent ∘ NEArray.toArray ∘ map renderListToken $
  tokens

renderList :: CSS -> List -> Html.PlainHTML
renderList css (OrderedList tokens) = renderListUsing (Html.ol [ style css ])
  tokens
renderList css (UnorderedList tokens) = renderListUsing (Html.ul [ style css ])
  tokens

isComment :: Element -> Boolean
isComment (ElementComment _) = true
isComment _ = false

renderAppTitle
  :: Path -> Document -> Maybe AppTitle
renderAppTitle path doc =
  case take 1 ∘ filter (not isComment) ∘ elements $ doc of
    [ ElementHeading (H1 span) ] ->
      Just ∘ AppTitle $
        { h1: [ renderSpan (pure unit) span ]
        , accessory:
            [ Button.renderPlain
                { children:
                    [ Html.a'
                        [ href ∘ unwrap ∘ Concept.Remote.humanUrl $ path
                        , target "_blank"
                        , rel "noopener noreferrer"
                        ]
                        [ Html.div
                            [ style Style.githubButton ]
                            [ Icon.render Icon.Github ]
                        ]
                    ]
                , styleButton: pure unit
                , styleContainer: pure unit
                , theme: Style.githubButtonTheme
                }
            ]
        , hash: hashSpan span
        }
    -- this should be unreachable
    -- TODO(orion): change the interface of Document to include
    -- a type-safe H1 (e.g. Document {heading :: Heading, body :: Array Element})
    _ -> Nothing

renderBody :: CSS -> Document -> Effect Html.PlainHTML
renderBody x d =
  let
    topLevelStyle 0 = Style.topLevelFirst
    topLevelStyle _ = Style.topLevel

    render' :: Int -> Element -> Effect Html.PlainHTML
    render' i (ElementList l) = pure $ renderList (topLevelStyle i) l
    render' i (ElementCodeFence cf) = renderCode (topLevelStyle i) cf
    render' i (ElementHeading h) = pure $ renderHeading (topLevelStyle i) h
    render' 0 (ElementSpan s) =
      pure
        $ renderSpan
            ( do
                Style.topLevelSpan
                Style.topLevelFirst
            )
            s
    render' _ (ElementSpan s) = pure $ renderSpan Style.topLevelSpan s
    render' _ _ = pure $ HH.div_ []

    body = drop 1 ∘ filter (not isComment)
  in
    do
      xs <- sequence
        ∘ mapWithIndex render'
        ∘ body
        ∘ elements
        $ d

      pure $ Html.div [ style (Style.documentBody >>= const x) ] xs
