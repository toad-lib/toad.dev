module Kwap.App.Css.Grid
  ( Col(..)
  , Row(..)
  , AppGridArea(..)
  , grid
  , gridArea
  , appGrid
  , inAppContent
  , inAppNavbar
  )
  where

import Prelude

import Kwap.App.Css as Css
import CSS.Common as Css.Common
import Data.Array as Array
import Data.Fist
import Data.Functor (class Functor)
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.String as String
import Kwap.App.Layout (AppLayout(..))

newtype Col s = Col (Css.Size s)

columnSize :: forall s. Col s -> Css.Size s
columnSize (Col s) = s

data Row :: (Type -> Type) -> Type -> Type -> Type
data Row t a s = Row (Css.Size s) (t a)

rowMembers :: ∀ t a s. Fist t => Row t a s -> t a
rowMembers (Row _ t) = t

rowSize :: ∀ t a s. Fist t => Row t a s -> Css.Size s
rowSize (Row s _) = s

gridArea :: ∀ a. (a -> String) -> a -> Css.CSS
gridArea f = f >>> Css.key (Css.fromString "grid-area")

grid :: ∀ t f a s. Fist t => Functor f => Foldable f => t (Col s) -> f (Row t a s) -> (a -> String) -> Css.CSS
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
appGrid _ = grid
            (fist1 $ Col (Css.pct >>> Css.anySize $ 100.0))
            [ Row (Css.rem >>> Css.anySize $ 20.0) $ fist1 AppGridNavbar
            , Row Css.Common.auto $ fist1 AppGridContent
            ]
            appGridLabel
