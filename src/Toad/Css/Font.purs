module Toad.Css.Font
  ( Font(..)
  , FontSize(..)
  , FontFamily(..)
  , font
  , fontSize
  , fontFamily
  , fontSizePt
  ) where

import Toad.Prelude

import CSS as Css
import CSS.Font as Css.Font
import Data.Array.NonEmpty as NEA

--| private
data FontWeight
  = Medium
  | Light
  | Semibold
  | Bold
  | ExtraBold

data FontSize
  = FontSizeDefault
  | FontSizeSmall
  | FontSizeBody
  | FontSizeHuge
  | FontSizeH1
  | FontSizeH2
  | FontSizeH3
  | FontSizeH4
  | FontSizeH5
  | FontSizeH6

data FontFamily
  = FontFamilyDefault
  | AtkinsonBold
  | AtkinsonMedium
  | LibreBaskervilleBold
  | WorkSansSemibold
  | WorkSansBold

data Font = Font FontFamily FontSize

weight :: FontFamily -> FontWeight
weight = case _ of
  FontFamilyDefault -> Medium
  AtkinsonMedium -> Medium
  AtkinsonBold -> Bold
  LibreBaskervilleBold -> Bold
  WorkSansSemibold -> Semibold
  WorkSansBold -> Bold

cssFontFamily :: FontFamily -> Css.CSS
cssFontFamily =
  let
    fallback = NEA.toNonEmpty ∘ pure

    atk = Css.fontFamily
      (pure "Atkinson Hyperlegible")
      (fallback Css.Font.sansSerif)
    bsk = Css.fontFamily
      (pure "Libre Baskerville")
      (fallback Css.Font.serif)
    wrk = Css.fontFamily
      (pure "Work Sans")
      (fallback Css.Font.sansSerif)
  in
    case _ of
      FontFamilyDefault -> cssFontFamily AtkinsonMedium
      AtkinsonBold -> atk
      AtkinsonMedium -> atk
      WorkSansSemibold -> wrk
      WorkSansBold -> wrk
      LibreBaskervilleBold -> bsk

cssFontWeight :: FontWeight -> Css.CSS
cssFontWeight = case _ of
  ExtraBold -> Css.fontWeight (Css.weight 900.0)
  Bold -> Css.fontWeight (Css.weight 700.0)
  Semibold -> Css.fontWeight (Css.weight 600.0)
  Medium -> Css.fontWeight (Css.weight 500.0)
  Light -> Css.fontWeight (Css.weight 300.0)

fontSizePt :: FontSize -> Css.Size Css.LengthUnit
fontSizePt =
  let
    pt = case _ of
      FontSizeDefault -> pt FontSizeBody
      FontSizeSmall -> 8.0
      FontSizeH6 -> 10.0
      FontSizeBody -> 16.0
      FontSizeH5 -> 12.0
      FontSizeH4 -> 16.0
      FontSizeH3 -> 24.0
      FontSizeH2 -> 32.0
      FontSizeH1 -> 48.0
      FontSizeHuge -> 72.0
  in
    pt >>> Css.pt

fontFamily :: FontFamily -> Font
fontFamily family = Font family mempty

fontSize :: FontSize -> Font
fontSize = Font mempty

font :: Font -> Css.CSS
font (Font family size) = do
  cssFontFamily family
  Css.fontSize $ fontSizePt size
  cssFontWeight $ weight family

instance fontSizeSemigroup :: Semigroup FontSize where
  append a b
    | b == mempty = a
    | otherwise = b

instance fontSizeMonoid :: Monoid FontSize where
  mempty = FontSizeDefault

derive instance fontSizeGeneric :: Generic FontSize _

instance fontSizeEq :: Eq FontSize where
  eq = genericEq

instance fontFamilySemigroup :: Semigroup FontFamily where
  append a b
    | b == mempty = a
    | otherwise = b

instance fontFamilyMonoid :: Monoid FontFamily where
  mempty = FontFamilyDefault

derive instance fontFamilyGeneric :: Generic FontFamily _

instance fontFamilyEq :: Eq FontFamily where
  eq = genericEq

instance fontSemigroup :: Semigroup Font where
  append (Font familyA sizeA) (Font familyB sizeB) =
    Font
      (familyA <> familyB)
      (sizeA <> sizeB)

instance fontMonoid :: Monoid Font where
  mempty = Font mempty mempty
