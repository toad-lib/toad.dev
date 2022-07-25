module Kwap.Atom.Cloud where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Halogen.Svg.Attributes as SvgA
import Halogen.Svg.Attributes.Color as SvgColor
import Halogen.Svg.Elements as Svg
import Kwap.Css as Css
import Kwap.Html (HTML)

newtype StrokeWidth = StrokeWidth Number

derive instance ntStrokeWidth :: Newtype StrokeWidth _

newtype Height = Height Number

derive instance ntHeight :: Newtype Height _

newtype Width = Width Number

derive instance ntWidth :: Newtype Width _

newtype Overlap = Overlap Number

derive instance ntOverlap :: Newtype Overlap _

render_ :: ∀ w i. HTML w i
render_ = render (Height 100.0) (Width 500.0) (StrokeWidth 5.0) (Overlap 0.4)

renderRects
  :: forall w i
   . { x :: Number, y :: Number, h :: Number, w :: Number }
  -> HTML w i
renderRects { x, y, h, w } = Svg.svg [] []

render :: ∀ w i. Height -> Width -> StrokeWidth -> Overlap -> HTML w i
render (Height h) (Width w) (StrokeWidth sw) (Overlap o) =
  let
    spread = 30.0
    rectHeight = (h * 0.5) + (h * 0.5 * o)
  in
    Svg.svg
      [ SvgA.viewBox 0.0 0.0 w h ]
      [ Svg.rect
          [ SvgA.fill $ SvgColor.Named "black"
          , SvgA.height h
          , SvgA.width w
          , SvgA.mask "url(#cloud_mask)"
          ]
      , Svg.rect
          [ SvgA.fill $ SvgColor.Named "white"
          , SvgA.x sw
          , SvgA.y sw
          , SvgA.height $ h - (sw * 2.0)
          , SvgA.width $ w - (sw * 2.0)
          , SvgA.mask "url(#cloud_mask2)"
          ]
      , Svg.mask [ SvgA.id "cloud_mask" ]
          [ Svg.rect
              [ SvgA.fill $ SvgColor.Named "white"
              , SvgA.x $ spread
              , SvgA.y $ h - rectHeight
              , SvgA.rx $ rectHeight / 2.0
              , SvgA.height rectHeight
              , SvgA.width $ w - spread
              ]
          , Svg.rect
              [ SvgA.fill $ SvgColor.Named "white"
              , SvgA.x 0.0
              , SvgA.y 0.0
              , SvgA.rx $ rectHeight / 2.0
              , SvgA.height rectHeight
              , SvgA.width $ w - spread
              ]
          ]
      , Svg.mask [ SvgA.id "cloud_mask2" ]
          [ Svg.rect
              [ SvgA.fill $ SvgColor.Named "white"
              , SvgA.x $ spread + sw
              , SvgA.y $ h - rectHeight + sw
              , SvgA.rx $ ((rectHeight - sw * 2.0) / 2.0)
              , SvgA.height $ rectHeight - (sw * 2.0)
              , SvgA.width $ w - spread - (sw * 2.0)
              ]
          , Svg.rect
              [ SvgA.fill $ SvgColor.Named "white"
              , SvgA.x sw
              , SvgA.y sw
              , SvgA.rx $ ((rectHeight - sw * 2.0) / 2.0)
              , SvgA.height $ rectHeight - (sw * 2.0)
              , SvgA.width $ w - spread - (sw * 2.0)
              ]
          ]
      ]
