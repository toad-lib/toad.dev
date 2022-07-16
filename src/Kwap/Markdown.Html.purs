module Kwap.Markdown.Html where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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

renderSpan :: forall w i. Span -> KH.HTML w i
renderSpan (Span ts) = KH.span [] (NEArray.toArray <<< map renderToken $ ts)

renderHeading :: forall w i. Heading -> KH.HTML w i
renderHeading (H1 s) = KH.h1_ [ renderSpan s ]
renderHeading (H2 s) = KH.h2_ [ renderSpan s ]
renderHeading (H3 s) = KH.h3_ [ renderSpan s ]
renderHeading (H4 s) = KH.h4_ [ renderSpan s ]
renderHeading (H5 s) = KH.h5_ [ renderSpan s ]
renderHeading (H6 s) = KH.h6_ [ renderSpan s ]

render :: forall w i. Document -> KH.HTML w i
render d =
  let
    render' (ElementComment _) = Nothing
    render' (ElementCodeFence _) = Nothing
    render' (ElementList _) = Nothing
    render' (ElementHeading h) = Just $ renderHeading h
    render' (ElementSpan s) = Just $ renderSpan s
  in
    KH.div_ <<< Array.catMaybes <<< map render' <<< elements $ d
