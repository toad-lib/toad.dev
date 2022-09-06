module Toad.Atom.Button where

import Toad.Prelude

import CSS.Cursor (pointer)
import CSS.Selector as Select
import CSS.Time as Time
import Data.Active (Active(..))
import Data.Color.OkLab (Lightness(..))
import Halogen.HTML.Events (onClick)
import Toad.Css (CSS, backgroundColor, black, border, colorFg, colorPrimary, colorPrimary2, colorPrimary3, colorPrimary4, colorPrimary5, cursor, definedIn, green, kwapEasing, oklab, padding, pseudo, px, select, solid, style, sym, transition)
import Toad.Html as Html

className :: String
className = "toad-button"

activeClassName :: String
activeClassName = "toad-button-active"

globalStyles :: CSS
globalStyles = do
  select
    ( Select.element "button"
        `Select.with` Select.byClass className
    )
    do
      border solid (px 0.0) black
      sym padding $ px 0.0
      cursor pointer
      backgroundColor ∘ oklab ∘ green $ Lightness 0.96
      transition "background-color" (Time.ms 75.0) kwapEasing (Time.ms 0.0)

  select
    ( Select.element "button"
        `Select.with` Select.byClass className
        `Select.with` pseudo "hover"
    )
    do
      definedIn "Toad.Atom.Button"
      backgroundColor ∘ oklab ∘ green $ Lightness 0.8

  let
    active = do
      definedIn "Toad.Atom.Button"
      backgroundColor ∘ oklab ∘ green $ Lightness 0.75

  select
    ( Select.element "button"
        `Select.with` Select.byClass className
        `Select.with` pseudo "active"
    )
    active

  select
    ( Select.element "button"
        `Select.with` Select.byClass activeClassName
    )
    active

render
  :: forall w i
   . Active
  -> Maybe CSS
  -> i
  -> Array (Html.HTML w i)
  -> Html.HTML w i
render a x i c = Html.button
  [ Html.classNames
      $ [ className ]
      <> case a of
        Active -> [ activeClassName ]
        Inactive -> []
  , style do
      maybe (pure unit) id x
  , onClick $ const i
  ]
  c
