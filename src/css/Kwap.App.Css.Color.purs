module Kwap.App.Css.Color
  ( Accel2D(..)
  , Color(..)
  , KwapGradient
  , Position2D(..)
  , Shade(..)
  , TwoDim(..)
  , Velocity2D(..)
  , backgroundColor
  , color
  , kwapGradient
  , kwapGradientInit
  , tick
  ) where

import Prelude

import CSS as Css
import CSS.Color as Css.Color
import CSS.Common as Css.Common
import CSS.Size as Css.Size
import Data.Array (snoc)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Number (max, min)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (fst)
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
  Lightest -> Css.Color.lighten 0.45 >>> Css.Color.saturate (-0.35)
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

data TwoDim a = TwoDim a a
newtype Position2D = Position2D (TwoDim Number)
newtype Velocity2D = Velocity2D (TwoDim Number)
newtype Accel2D = Accel2D (TwoDim Number)

type KwapGradient = Array
  { color :: Color
  , pos :: Position2D
  , vel :: Velocity2D
  , acc :: Accel2D
  }

kwapGradientInit :: KwapGradient
kwapGradientInit =
  [ { color: Pink Medium
    , pos: Position2D $ TwoDim 0.0 100.0
    , vel: Velocity2D $ TwoDim 0.0 0.0
    , acc: Accel2D $ TwoDim 0.0 0.0
    }
  , { color: Yellow Medium
    , pos: Position2D $ TwoDim 0.0 0.0
    , vel: Velocity2D $ TwoDim 0.0 0.0
    , acc: Accel2D $ TwoDim 0.0 0.0
    }
  , { color: Purple Medium
    , pos: Position2D $ TwoDim 100.0 0.0
    , vel: Velocity2D $ TwoDim 0.0 0.0
    , acc: Accel2D $ TwoDim 0.0 0.0
    }
  , { color: Red Light
    , pos: Position2D $ TwoDim 50.0 50.0
    , vel: Velocity2D $ TwoDim 0.0 0.0
    , acc: Accel2D $ TwoDim 0.0 0.0
    }
  ]

accelField :: Position2D -> Accel2D
accelField (Position2D (TwoDim x y)) =
  let
    x' = (x - 50.0)
    y' = (y - 50.0)
  in
    Accel2D $ TwoDim ((y' - x') / 2000.0) ((-x' - y') / 2000.0)

velocity :: Accel2D -> Velocity2D -> Velocity2D
velocity (Accel2D (TwoDim ax ay)) (Velocity2D (TwoDim vx vy)) = Velocity2D
  (TwoDim (vx + ax) (vy + ay))

position :: Velocity2D -> Position2D -> Position2D
position (Velocity2D (TwoDim vx vy)) (Position2D (TwoDim x y)) = Position2D
  (TwoDim (x + vx) (y + vy))

tick :: KwapGradient -> KwapGradient
tick =
  let
    tickOne { color: color', pos, vel } =
      let
        acc' = accelField pos
        vel' = velocity acc' vel
        clamp = max 0.0 >>> min 100.0
        clampPos (Position2D (TwoDim x y)) = Position2D
          (TwoDim (clamp x) (clamp y))
      in
        { color: color'
        , pos: clampPos $ position vel' pos
        , vel: vel'
        , acc: acc'
        }
  in
    map tickOne

kwapGradient :: KwapGradient -> Css.CSS
kwapGradient gradients =
  let
    transparent = Css.Color.hsla 0.0 0.0 0.0 0.0

    prefixed = Css.value >>> (\(Css.Value p) -> p)

    center :: Css.Side
    center = Css.Common.other $ (Css.fromString "center")

    side_ :: String -> Css.Size Css.Percentage -> Css.Side
    side_ side =
      Css.Size.sizeToString
        >>> ((side <> " ") <> _)
        >>> Css.fromString
        >>> Css.Common.other

    sideBottom_ = side_ "bottom"
    sideLeft_ = side_ "left"

    background prefix =
      let
        radialGradient
          :: ∀ a. Css.Loc a => a -> Css.Radial -> Css.Ramp -> String
        radialGradient l rd =
          Css.radialGradient l rd
            >>> prefixed
            >>> Css.plain
            >>> (prefix <> _)

        alpha a = Css.Color.toHSLA >>>
          (\({ h, s, l }) -> Css.Color.hsla h s l a)

        render { color: color', pos: Position2D (TwoDim left bottom) } =
          radialGradient
            (sideLeft_ (Css.pct left) /\ sideBottom_ (Css.pct bottom))
            circle
            [ (alpha 0.8 $ cssColor color') /\ Css.pct 0.0
            , transparent /\ Css.pct 100.0
            ]

        circle :: Css.Radial
        circle = Css.Common.other $ Css.fromString "150% 150%"

        solidBackground =
          radialGradient
            center
            circle
            [ cssColor (Purple Light) /\ Css.pct 0.0
            , cssColor (Purple Light) /\ Css.pct 100.0
            ]
      in
        Css.key
          (Css.fromString "background-image")
          $ ((flip snoc) solidBackground >>> String.joinWith ", ")
              (render <$> gradients)

    vendorPrefixes = case Css.Common.browsers of
      Css.Prefixed arr -> fst <$> arr
      _ -> []
  in
    for_ vendorPrefixes $ \vendorPrefix -> do
      background vendorPrefix

derive instance genericColor :: Generic Color _
derive instance genericShade :: Generic Shade _
derive instance genericTwoDim :: Generic (TwoDim a) _
derive instance genericAccel2D :: Generic Accel2D _
derive instance genericVelocity2D :: Generic Velocity2D _
derive instance genericPosition2D :: Generic Position2D _

instance showColor :: Show Color where
  show = genericShow

instance showShade :: Show Shade where
  show = genericShow

instance showTwoDim :: Show a => Show (TwoDim a) where
  show = genericShow

instance showAccel2D :: Show Accel2D where
  show = genericShow

instance showVelocity2D :: Show Velocity2D where
  show = genericShow

instance showPosition2D :: Show Position2D where
  show = genericShow
