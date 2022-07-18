module Kwap.Markdown.Html where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as HP
import Kwap.Css (CSS, FontFamily(..), fontFamily)
import Kwap.Html as KH
import Kwap.Markdown
  ( Anchor(..)
  , Document
  , Element(..)
  , Heading(..)
  , Span(..)
  , Text(..)
  , Token(..)
  , elements
  )
import Kwap.Markdown.Html.Style as Style
import Kwap.Route as Route

renderText :: forall w i. Text -> KH.HTML w i
renderText (Unstyled s) = KH.text s
renderText (Bold s) = KH.b_ <<< pure <<< KH.text $ s
renderText (Italic s) = KH.i_ <<< pure <<< KH.text $ s
renderText (BoldItalic s) = KH.b_ <<< pure <<< KH.i_ <<< pure <<< KH.text $ s
renderText (InlineCode s) = KH.code_ <<< pure <<< KH.text $ s

renderAnchor :: forall w i. Anchor -> KH.HTML w i
renderAnchor (Anchor s u) =
  HH.a
    [ HP.href u ]
    [ KH.span_ <<< NEArray.toArray <<< map renderText $ s ]
renderAnchor (ConceptAnchor s i) =
  KH.a_
    (Route.Concepts $ Route.One i)
    (pure <<< KH.span_ <<< NEArray.toArray <<< map renderText $ s)

renderToken :: forall w i. Token -> KH.HTML w i
renderToken (TextToken t) = renderText t
renderToken (AnchorToken a) = renderAnchor a

maybeStyle :: forall r i. Maybe CSS -> IProp (style :: String | r) i
maybeStyle = style <<< fromMaybe (pure unit)

renderSpan :: forall w i. Maybe CSS -> Span -> KH.HTML w i
renderSpan s (Span ts) = KH.span [ maybeStyle s ]
  (NEArray.toArray <<< map renderToken $ ts)

renderHeading :: forall w i. Maybe CSS -> Heading -> KH.HTML w i
renderHeading x =
  let
    style' f = style do
      fromMaybe (pure unit) x
      KH.headingStyle f
  in
    case _ of
      (H1 s) -> KH.h1 [ style' $ KH.h1Font ] [ renderSpan mempty s ]
      (H2 s) -> KH.h2 [ style' $ KH.h2Font ] [ renderSpan mempty s ]
      (H3 s) -> KH.h3 [ style' $ KH.h3Font ] [ renderSpan mempty s ]
      (H4 s) -> KH.h4 [ style' $ KH.h4Font ] [ renderSpan mempty s ]
      (H5 s) -> KH.h5 [ style' $ KH.h5Font ] [ renderSpan mempty s ]
      (H6 s) -> KH.h6 [ style' $ KH.h6Font ] [ renderSpan mempty s ]

renderable :: Element -> Maybe Element
renderable (ElementComment _) = Nothing
renderable (ElementCodeFence _) = Nothing
renderable (ElementList _) = Nothing
renderable e = Just e

render :: forall w i. Document -> KH.HTML w i
render d =
  let
    style' 0 = Style.elementFirst
    style' _ = Style.element

    render' _ (ElementComment _) = HH.div_ []
    render' _ (ElementCodeFence _) = HH.div_ []
    render' _ (ElementList _) = HH.div_ []
    render' i (ElementHeading h) = renderHeading (Just $ style' i) h
    render' i (ElementSpan s) =
      renderSpan
        ( Just do
            Style.span
            style' i
        )
        s
  in
    KH.div [ style Style.document ]
      <<< mapWithIndex render'
      <<< filterMap renderable
      <<< elements $ d
