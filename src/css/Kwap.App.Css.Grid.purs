module Kwap.App.Css.Grid
  ( AppGridArea(..)
  , AppLayout(..)
  , area
  , container
  , occupyContent
  , occupyNavbar
  )
  where

import Prelude

import CSS as Css
import Data.String as String

data AppGridArea
  = AppGridNavbar
  | AppGridContent

data AppLayout
  = AppLayoutVertical
  | AppLayoutHorizontal

gridAreas :: Array AppGridArea
gridAreas = [ AppGridNavbar, AppGridContent ]

gridAreaSize :: ∀ a. AppGridArea -> Css.Value
gridAreaSize = case _ of
  AppGridNavbar -> Css.value $ Css.rem 20.0
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

container :: AppLayout -> Css.CSS
container layout =
  let
    appAreaTemplate area' =
      [ Css.fromString $ "[" <> (gridAreaLabel area') <> "]"
      , Css.value (gridAreaSize area')
      ] # Css.noCommas
    appTemplate = gridAreas <#> appAreaTemplate # Css.noCommas

    deadTemplate =
      [ Css.fromString $ "["
          <> (gridAreas <#> gridAreaLabel # String.joinWith " ")
          <> "]"
      , Css.value (Css.pct 100.0)
      ] # Css.noCommas

    rows = case layout of
             AppLayoutHorizontal -> deadTemplate
             AppLayoutVertical -> appTemplate

    cols = case layout of
             AppLayoutHorizontal -> appTemplate
             AppLayoutVertical -> deadTemplate
  in
    do
      Css.display Css.grid
      Css.key (Css.fromString "grid-template-rows") rows
      Css.key (Css.fromString "grid-template-columns") cols
