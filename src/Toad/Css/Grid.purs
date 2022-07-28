module Toad.Css.Grid
  ( GridCol(..)
  , GridRow(..)
  , AppGridArea(..)
  , grid
  , gridArea
  , appGrid
  , inAppContent
  , inAppNavbar
  ) where

import Data.Fist
import Prelude

import CSS.Common as Css.Common
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, intercalate)
import Data.Functor (class Functor)
import Data.String as String
import Toad.Css as Css
import Toad.Layout (AppLayout(..))

newtype GridCol s = GridCol (Css.Size s)

columnSize :: forall s. GridCol s -> Css.Size s
columnSize (GridCol s) = s

data GridRow :: (Type -> Type) -> Type -> Type -> Type
data GridRow t a s = GridRow (Css.Size s) (t a)

rowMembers :: ∀ t a s. Fist t => GridRow t a s -> t a
rowMembers (GridRow _ t) = t

rowSize :: ∀ t a s. Fist t => GridRow t a s -> Css.Size s
rowSize (GridRow s _) = s

gridArea :: ∀ a. (a -> String) -> a -> Css.CSS
gridArea f = f >>> Css.key (Css.fromString "grid-area")

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
    templateRows = rows
      <#> rowSize
      # Array.fromFoldable
      # Css.noCommas

    templateColumns = columns
      # arrayOf
      <#> columnSize
      # Css.noCommas
    template = rows
      <#> rowMembers
        >>> arrayOf
        >>> map label
        >>> intercalate " "
        >>> append "\""
        >>> flip append "\""
        >>> Css.value
      # Array.fromFoldable
      # Css.noCommas
  in
    do
      Css.display Css.grid

      (Css.fromString >>> Css.key)
        "grid-template-areas"
        template
      (Css.fromString >>> Css.key)
        "grid-template-rows"
        templateRows
      (Css.fromString >>> Css.key)
        "grid-template-columns"
        templateColumns

data AppGridArea
  = AppGridNavbar
  | AppGridContent

appGridLabel :: AppGridArea -> String
appGridLabel = case _ of
  AppGridNavbar -> "navbar"
  AppGridContent -> "content"

inAppNavbar :: Css.CSS
inAppNavbar = gridArea appGridLabel AppGridNavbar

inAppContent :: Css.CSS
inAppContent = gridArea appGridLabel AppGridContent

appGrid :: AppLayout -> Css.CSS
appGrid = case _ of
  AppLayoutDesktop -> appGridDesktop
  AppLayoutMobile -> appGridMobile

appGridMobile :: Css.CSS
appGridMobile =
  let
    rem = Css.rem >>> Css.anySize
    pct = Css.pct >>> Css.anySize
  in
    grid
      (fist1 $ GridCol (pct 100.0))
      [ GridRow (rem 20.0) $ fist1 AppGridNavbar
      , GridRow Css.Common.auto $ fist1 AppGridContent
      ]
      appGridLabel

appGridDesktop :: Css.CSS
appGridDesktop =
  let
    rem = Css.rem >>> Css.anySize
    pct = Css.pct >>> Css.anySize
  in
    grid
      (fist2 (GridCol (rem 20.0)) (GridCol Css.Common.auto))
      [ GridRow (pct 100.0) (fist2 AppGridNavbar AppGridContent)
      ]
      appGridLabel
