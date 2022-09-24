module Toad.Markdown.Html where

import Toad.Prelude

import Data.Array (concat, drop, take)
import Data.Array.NonEmpty as NEArray
import Data.Filterable (filter, filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Toad.Css (CSS, green)
import Toad.Html as Html
import Toad.Markdown
  ( Anchor(..)
  , Document
  , Element(..)
  , Heading(..)
  , Span(..)
  , Text(..)
  , Token(..)
  , elements
  )
import Toad.Markdown.Html.Style as Style
import Toad.Route as Route

renderText :: ∀ w i. Text -> Html.HTML w i
renderText (Unstyled s) = Html.text s
renderText (Bold s) = Html.b_ [ renderText ∘ Unstyled $ s ]
renderText (Italic s) = Html.i_ [ renderText ∘ Unstyled $ s ]
renderText (BoldItalic s) = Html.b_ [ renderText ∘ Italic $ s ]
renderText (InlineCode s) = Html.code_ [ renderText ∘ Unstyled $ s ]

renderAnchor :: ∀ w i. Anchor -> Html.HTML w i
renderAnchor (Anchor s u) =
  HH.a
    [ HP.href u ]
    [ Html.span [ style Style.anchorSpan ] ∘ NEArray.toArray ∘ map renderText $
        s
    ]
renderAnchor (ConceptAnchor s i) =
  Html.a_
    (Route.Concepts $ Route.One i)
    [ Html.span [ style Style.anchorSpan ] ∘ NEArray.toArray ∘ map renderText $
        s
    ]

renderToken :: ∀ w i. Token -> Html.HTML w i
renderToken (TextToken t) = renderText t
renderToken (AnchorToken a) = renderAnchor a

maybeStyle :: ∀ r i. Maybe CSS -> IProp (style :: String | r) i
maybeStyle = style ∘ fromMaybe (pure unit)

renderSpan :: ∀ w i. Maybe CSS -> Span -> Html.HTML w i
renderSpan s (Span ts) = Html.span
  [ maybeStyle s ]
  (NEArray.toArray ∘ map renderToken $ ts)

renderHeading :: ∀ w i. Maybe CSS -> Heading -> Html.HTML w i
renderHeading x =
  let
    style' f = style do
      fromMaybe (pure unit) x
      Html.headingStyle f
    span' = renderSpan mempty
  in
    case _ of
      H1 s -> Html.h1 [ style' Html.h1Font ] [ span' s ]
      H2 s -> Html.h2 [ style' Html.h2Font ] [ span' s ]
      H3 s -> Html.h3 [ style' Html.h3Font ] [ span' s ]
      H4 s -> Html.h4 [ style' Html.h4Font ] [ span' s ]
      H5 s -> Html.h5 [ style' Html.h5Font ] [ span' s ]
      H6 s -> Html.h6 [ style' Html.h6Font ] [ span' s ]

renderHeader :: ∀ w i. Span -> Html.HTML w i
renderHeader s =
  Html.div
    [ style Style.headerContainer
    ]
    [ Html.h1
        [ Style.headerClass ]
        [ renderSpan mempty s ]
    ]
renderHeader _ = Html.text "first element was not <h1>"

shouldSkip :: Element -> Boolean
shouldSkip (ElementComment _) = true
shouldSkip (ElementCodeFence _) = true
shouldSkip (ElementList _) = true
shouldSkip _ = false

isComment :: Element -> Boolean
isComment (ElementComment _) = true
isComment _ = false

render :: ∀ w i. Document -> Html.HTML w i
render d =
  let
    -- Element at index 0 will always be an h1.
    style' 0 = Style.elementFirst
    style' _ = Style.element

    render' _ (ElementComment _) = HH.div_ []
    render' _ (ElementCodeFence _) = HH.div_ []
    render' _ (ElementList _) = HH.div_ []
    render' i (ElementHeading h) =
      renderHeading
        ( Just do
            Style.topLevel
            style' i
        )
        h
    render' i (ElementSpan s) =
      renderSpan
        ( Just do
            Style.span
            Style.topLevel
            style' i
        )
        s
  in
    Html.div
      [ style Style.document ]
      [ case take 1 ∘ filter (not isComment) ∘ elements $ d of
          [ ElementHeading (H1 s) ] -> renderHeader s
          _ -> HH.text "unreachable"
      , Html.div
          [ style Style.documentBody ]
          ( mapWithIndex render'
              ∘ filter (not shouldSkip)
              ∘ drop 1
              ∘ filter (not isComment)
              ∘ elements
              $ d
          )
      ]
