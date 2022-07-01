module Kwap.App.Css.Color
  ( Accel2D(..)
  , Color(..)
  , KwapGradient
  , Position2D(..)
  , Shade(..)
  , Velocity2D(..)
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
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Number (max, min)
import Data.Profunctor.Strong (first, second)
import Data.Range (clamp, (..=))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..), fst)
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
  Light -> Css.Color.lighten 0.25 >>> Css.Color.saturate (-0.2)
  Medium -> Css.Color.lighten 0.0
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

newtype Position2D = Position2D (Tuple Number Number)
newtype Velocity2D = Velocity2D (Tuple Number Number)
newtype Accel2D = Accel2D (Tuple Number Number)

type KwapGradient = Array
  { color :: Color
  , pos :: Position2D
  , vel :: Velocity2D
  , acc :: Accel2D
  }

kwapGradientInit :: KwapGradient
kwapGradientInit =
  [ { color: Pink Medium
    , pos: Position2D $ Tuple 10.0 100.0
    , vel: Velocity2D $ Tuple 0.0 0.0
    , acc: Accel2D $ Tuple 0.0 0.0
    }
  , { color: Yellow Medium
    , pos: Position2D $ Tuple 10.0 10.0
    , vel: Velocity2D $ Tuple 0.0 0.0
    , acc: Accel2D $ Tuple 0.0 0.0
    }
  , { color: Purple Medium
    , pos: Position2D $ Tuple 100.0 15.0
    , vel: Velocity2D $ Tuple 0.0 0.0
    , acc: Accel2D $ Tuple 0.0 0.0
    }
  , { color: Purple Medium
    , pos: Position2D $ Tuple 100.0 90.0
    , vel: Velocity2D $ Tuple 0.0 0.0
    , acc: Accel2D $ Tuple 0.0 0.0
    }
  , { color: Red Light
    , pos: Position2D $ Tuple 45.0 55.0
    , vel: Velocity2D $ Tuple 0.0 0.0
    , acc: Accel2D $ Tuple 0.0 0.0
    }
  ]

class Cartesian x where
  coords :: x -> Tuple Number Number
  fromCoords :: Tuple Number Number -> x

instance cartesianAccel2D :: Cartesian Accel2D where
  coords (Accel2D t) = t
  fromCoords t = Accel2D t

instance cartesianVelocity2D :: Cartesian Velocity2D where
  coords (Velocity2D t) = t
  fromCoords t = Velocity2D t

instance cartesianPosition2D :: Cartesian Position2D where
  coords (Position2D t) = t
  fromCoords t = Position2D t

integral :: ∀ df f. Cartesian df => Cartesian f => df -> f -> f
integral df f =
  let
    df' = coords df
    f' = coords f
    go (Tuple dx dy) (Tuple x y) = fromCoords $ (dx + x) /\ (dy + y)
  in
    go df' f'

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

--| acceleration vector field
accelField :: Position2D -> Accel2D
accelField (Position2D (Tuple x y)) =
  let
    -- move the origin to the middle (50%, 50%)
    x' = (x - 50.0)
    y' = (y - 50.0)
    slowDownFactor = 150.0
  in
    Accel2D $ ((y' - x') / slowDownFactor) /\ ((-x' - y') / slowDownFactor)

tick :: KwapGradient -> KwapGradient
tick =
  let
    tickOne { color: color', pos, vel } =
      let
        acc' = accelField pos
        vel' = integral acc' vel
      in
        { color: color'
        , pos: fromCoords
            $ bimap (clamp $ (0.0) ..= 100.0) (clamp $ (0.0) ..= 100.0)
            $ coords
            $ integral vel' pos
        , vel: vel'
        , acc: acc'
        }
  in
    map tickOne

tupleValue :: ∀ a b. Css.Val a => Css.Val b => Tuple a b -> Css.Value
tupleValue = first Css.value >>> second Css.value >>> Css.value

kwapGradient :: KwapGradient -> Css.CSS
kwapGradient gradients =
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

        gradientCss { color: color', pos: Position2D (Tuple left bottom) } =
          radialGradient
            (sideLeft_ (Css.pct left) /\ sideBottom_ (Css.pct bottom))
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

derive instance genericColor :: Generic Color _
derive instance genericShade :: Generic Shade _
derive instance genericAccel2D :: Generic Accel2D _
derive instance genericVelocity2D :: Generic Velocity2D _
derive instance genericPosition2D :: Generic Position2D _

instance showColor :: Show Color where
  show = genericShow

instance showShade :: Show Shade where
  show = genericShow

instance showAccel2D :: Show Accel2D where
  show = genericShow

instance showVelocity2D :: Show Velocity2D where
  show = genericShow

instance showPosition2D :: Show Position2D where
  show = genericShow
