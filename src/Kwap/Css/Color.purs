module Kwap.Css.Color
  ( Color(..)
  , KwapGradient
  , Shade(..)
  , backgroundColor
  , color
  , cssColor
  , kwapGradient
  , kwapGradientInit
  , tick
  ) where

import Prelude

import CSS as Css
import CSS.Color as Css.Color
import CSS.Common as Css.Common
import CSS.Gradient as Css.Gradient
import CSS.Size as Css.Size
import Data.Array (snoc)
import Data.Bifunctor (bimap)
import Data.Coord.Cart as Cart
import Data.Coord.Polar (Pos(..), Radians(..))
import Data.Coord.Polar as Polar
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Number (max, min, pi, pow, sqrt)
import Data.Profunctor.Strong (first, second)
import Data.Range (clamp, (..=))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..), fst, swap)
import Data.Tuple.Nested ((/\))

data Shade
  = Lightest
  | Light
  | Medium
  | Dark
  | Darkest

data Color
  = Purple Shade
  | Yellow Shade
  | Pink Shade
  | Red Shade

shade :: Color -> Shade
shade = case _ of
  Yellow s -> s
  Purple s -> s
  Pink s -> s
  Red s -> s

shadify :: Shade -> Css.Color -> Css.Color
shadify = case _ of
  Lightest -> Css.Color.lighten 0.45 >>> Css.Color.saturate (-0.2)
  Light -> Css.Color.lighten 0.25 >>> Css.Color.saturate (0.2)
  Medium -> identity
  Dark -> Css.Color.darken 0.2 >>> Css.Color.saturate 0.2
  Darkest -> Css.Color.darken 0.4 >>> Css.Color.saturate 0.4

hue :: Color -> Number
hue = case _ of
  Yellow _ -> 15.0
  Purple _ -> 258.0
  Pink _ -> 328.0
  Red _ -> 360.0

sat :: Color -> Number
sat = case _ of
  Yellow _ -> 0.4
  Purple _ -> 0.5
  Pink _ -> 0.5
  Red _ -> 0.3

cssColor :: Color -> Css.Color.Color
cssColor color' = Css.Color.hsla (hue color') (sat color') 0.5 1.0 # shadify
  (shade color')

color :: Color -> Css.CSS
color = cssColor >>> Css.color

backgroundColor :: Color -> Css.CSS
backgroundColor = cssColor >>> Css.backgroundColor

newtype KwapGradient = KwapGradient
  ( Array
      { color :: Color
      , p :: Pos
      , da :: Delta Radians
      }
  )

derive newtype instance showKwapGradient :: Show KwapGradient
derive newtype instance eqKwapGradient :: Eq KwapGradient

newtype Delta a = Delta a

derive newtype instance showDelta :: Show a => Show (Delta a)
derive newtype instance eqDelta :: Eq a => Eq (Delta a)

delta :: forall a. Delta a -> a
delta (Delta a) = a

kwapGradientInit :: KwapGradient
kwapGradientInit =
  let
    da = Delta $ Radians $ pi / 64.0
  in
    KwapGradient $
      [ { color: Pink Medium
        , p: Polar.make 60.0 (Radians 0.0)
        , da
        }
      , { color: Yellow Medium
        , p: Polar.make 60.0 (Radians (pi / 2.0))
        , da
        }
      , { color: Purple Medium
        , p: Polar.make 60.0 (Radians pi)
        , da
        }
      , { color: Purple Medium
        , p: Polar.make 60.0 (Radians (3.0 * pi / 2.0))
        , da
        }
      , { color: Red Light
        , p: Polar.make 0.0 (Radians 0.0)
        , da: Delta (Radians 0.0)
        }
      ]

alpha :: Number -> Css.Color -> Css.Color
alpha a = Css.Color.toHSLA >>>
  (\({ h, s, l }) -> Css.Color.hsla h s l a)

transparent :: Css.Color
transparent = Css.Color.hsla 0.0 0.0 0.0 0.0

circle :: Css.Radial
circle = Css.Common.other $ Css.fromString "100% 100%"

center :: Css.Side
center = Css.Common.other $ (Css.fromString "center")

side_ :: String -> Css.Size Css.Percentage -> Css.Side
side_ side =
  Css.Size.sizeToString
    >>> ((side <> " ") <> _)
    >>> Css.fromString
    >>> Css.Common.other

sideBottom_ :: Css.Size Css.Percentage -> Css.Side
sideBottom_ = side_ "bottom"

sideLeft_ :: Css.Size Css.Percentage -> Css.Side
sideLeft_ = side_ "left"

tick :: KwapGradient -> KwapGradient
tick (KwapGradient grads) =
  let
    tickOne { color: color', p, da } =
      { color: color'
      , p: Polar.modifyAngle (_ + (delta da)) p
      , da
      }
  in
    KwapGradient $ tickOne <$> grads

tupleValue :: ∀ a b. Css.Val a => Css.Val b => Tuple a b -> Css.Value
tupleValue = first Css.value >>> second Css.value >>> Css.value

kwapGradient :: KwapGradient -> Css.CSS
kwapGradient (KwapGradient gradients) =
  let
    kwapGradientWithPrefix prefix =
      let
        radialGradient
          :: ∀ a. Css.Loc a => a -> Css.Radial -> Css.Ramp -> Css.Value
        radialGradient l rd ramp' =
          [ Css.fromString (prefix <> "radial-gradient(")
          , Css.noCommas
              [ Css.value rd
              , Css.fromString "at"
              , Css.value l
              , Css.fromString ","
              , ramp' <#> tupleValue # Css.value
              ]
          , Css.fromString ")"
          ] # Css.noCommas

        cssScalarFromPolar
          :: (Cart.Pos -> Number)
          -> Polar.Pos
          -> Css.Size.Size Css.Size.Percentage
        cssScalarFromPolar f = Css.pct <<< f <<< Cart.fromPolar

        gradientCss { color: color', p } =
          radialGradient
            ( sideLeft_
                (cssScalarFromPolar (Cart.x >>> (_ + 50.0)) p) /\ sideBottom_
                (cssScalarFromPolar (Cart.y >>> (_ + 50.0)) p)
            )
            circle
            [ (alpha 0.8 $ cssColor color') /\ Css.pct 0.0
            , transparent /\ Css.pct 100.0
            ]

        solidBackground =
          radialGradient
            center
            circle
            [ cssColor (Pink Medium) /\ Css.pct 0.0
            , cssColor (Pink Medium) /\ Css.pct 100.0
            ]

        atEnd = flip snoc
      in
        Css.key
          (Css.fromString "background-image")
          (gradients <#> gradientCss # atEnd solidBackground # Css.value)

    vendorPrefixes = case Css.Common.browsers of
      Css.Prefixed arr -> fst <$> arr
      _ -> []
  in
    for_ vendorPrefixes $ \vendorPrefix -> do
      kwapGradientWithPrefix vendorPrefix

derive instance eqColor :: Eq Color
derive instance eqShade :: Eq Shade
derive instance genericColor :: Generic Color _
derive instance genericShade :: Generic Shade _

instance showColor :: Show Color where
  show = genericShow

instance showShade :: Show Shade where
  show = genericShow
