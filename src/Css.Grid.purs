module Css.Grid
  ( GridCol(..)
  , GridRow(..)
  , grid
  , gridRowStart
  , gridArea
  ) where

import Prelude

import CSS as Css
import Data.Array as Array
import Data.Fist (class Fist)
import Data.Fist as Fist
import Data.Foldable (class Foldable, intercalate)

newtype GridCol s = GridCol (Css.Size s)

columnSize :: ∀ s. GridCol s -> Css.Size s
columnSize (GridCol s) = s

data GridRow :: (Type -> Type) -> Type -> Type -> Type
data GridRow t a s = GridRow (Css.Size s) (t a)

rowAreas :: ∀ t a s. Fist t => GridRow t a s -> t a
rowAreas (GridRow _ t) = t

rowSize :: ∀ t a s. Fist t => GridRow t a s -> Css.Size s
rowSize (GridRow s _) = s

gridArea :: ∀ a. (a -> String) -> a -> Css.CSS
gridArea f = Css.key (Css.fromString "grid-area") <<< f

gridRowStart :: ∀ a. (a -> String) -> a -> Css.CSS
gridRowStart f = Css.key (Css.fromString "grid-row-start") <<< f

grid
  :: ∀ t f a s
   . Fist t
  => Functor f
  => Foldable f
  => t (GridCol s)
  -> f (GridRow t a s)
  -> (a -> String)
  -> Css.CSS
grid columns rows label =
  let
    rowSizes = Css.noCommas <<< Array.fromFoldable <<< map rowSize $ rows

    columnSizes = Css.noCommas <<< map columnSize <<< Fist.toArray $ columns

    templateLabels =
      Css.noCommas
        <<< Array.fromFoldable
        <<< map
          ( Css.value
              <<< ("\"" <> _)
              <<< (_ <> "\"")
              <<< intercalate " "
              <<< map label
              <<< Fist.toArray
              <<< rowAreas
          )
        $ rows

    key = Css.key <<< Css.fromString
  in
    do
      Css.display Css.grid

      key "grid-template-areas" templateLabels
      key "grid-template-rows" rowSizes
      key "grid-template-columns" columnSizes

