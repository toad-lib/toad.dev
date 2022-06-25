module Kwap.App.Css.Grid
  ( AppGridArea(..)
  , area
  , container
  , occupyContent
  , occupyNavbar
  ) where

import Prelude

import CSS as Css
import Data.String as String

data AppGridArea
  = AppGridNavbar
  | AppGridContent

gridAreas :: Array AppGridArea
gridAreas = [ AppGridNavbar, AppGridContent ]

gridAreaSize :: ∀ a. AppGridArea -> Css.Value
gridAreaSize = case _ of
  AppGridNavbar -> Css.value $ Css.rem 8.0
  AppGridContent -> Css.fromString "auto"

gridAreaLabel :: AppGridArea -> String
gridAreaLabel = case _ of
  AppGridNavbar -> "navbar"
  AppGridContent -> "content"

area :: AppGridArea -> Css.CSS
area = gridAreaLabel >>> Css.key (Css.fromString "grid-area")

occupyNavbar :: Css.CSS
occupyNavbar = area AppGridNavbar

occupyContent :: Css.CSS
occupyContent = area AppGridContent

container :: Css.CSS
container =
  let
    rowTemplate area =
      [ Css.fromString $ "[" <> (gridAreaLabel area) <> "]"
      , Css.value (gridAreaSize area)
      ] # Css.noCommas
    columnTemplate =
      [ Css.fromString $ "["
          <> (gridAreas <#> gridAreaLabel # String.joinWith " ")
          <> "]"
      , Css.value (Css.pct 100.0)
      ] # Css.noCommas
  in
    do
      Css.display Css.grid
      Css.key (Css.fromString "grid-template-rows")
        (gridAreas <#> rowTemplate # Css.noCommas)
      Css.key (Css.fromString "grid-template-columns") columnTemplate
