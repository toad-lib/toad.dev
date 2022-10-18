module Toad.Atom.Button.Style
  ( button
  , className
  , activeClassName
  , Theme(..)
  , primary
  ) where

import Toad.Css
import Toad.Prelude

import CSS.Cursor (pointer)
import CSS.Selector as Select
import CSS.Time as Time
import Data.Color.OkLab as OkLab

data Theme = Theme
  { color :: OkLab.Lightness -> OkLab.Lab
  , inert :: OkLab.Lightness
  , active :: OkLab.Lightness
  , hover :: OkLab.Lightness
  }

primary :: (OkLab.Lightness -> OkLab.Lab) -> Theme
primary color = Theme
  { color
  , inert: OkLab.Lightness 0.96
  , active: OkLab.Lightness 0.75
  , hover: OkLab.Lightness 0.90
  }

className :: String
className = "button"

activeClassName :: String
activeClassName = "button-active"

button :: Theme -> CSS
button (Theme { color, inert: lInert, active: lActive, hover: lHover }) = do
  select (Select.element "button" `Select.with` Select.byClass className)
    do
      height $ pct 100.0
      width $ pct 100.0
      border solid (px 0.0) black
      sym padding $ px 0.0
      cursor pointer
      backgroundColor ∘ oklab ∘ color $ lInert
      transition "background-color" (Time.ms 75.0) kwapEasing (Time.ms 0.0)

  select
    ( Select.element "button"
        `Select.with` Select.byClass className
        `Select.with` pseudo "hover"
    )
    (backgroundColor ∘ oklab ∘ color $ lHover)

  select
    ( Select.element "button"
        `Select.with` Select.byClass className
        `Select.with` pseudo "active"
    )
    (backgroundColor ∘ oklab ∘ color $ lActive)

  select
    ( Select.element "button"
        `Select.with` Select.byClass activeClassName
    )
    (backgroundColor ∘ oklab ∘ color $ lActive)
