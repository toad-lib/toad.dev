module Toad.Atom.AppTitle.Style
  ( global
  , backgroundLightness
  , container
  , h1Class
  ) where

import Toad.Prelude

import CSS (justifyContent, paddingRight)
import CSS.Common (center)
import CSS.Selector as Select
import Data.Color.OkLab (Lightness(..))
import Toad.Css
  ( CSS
  , alignItems
  , backgroundColor
  , color
  , definedIn
  , display
  , flex
  , green
  , oklab
  , paddingLeft
  , rem
  , select
  , spaceBetween
  )
import Toad.Html as Html

backgroundLightness :: Lightness
backgroundLightness = Lightness 0.75

global :: CSS
global = do
  definedIn "Toad.Atom.AppTitle.Style"
  select
    ( Select.element "h1"
        `Select.with` Select.byClass h1ClassString
    )
    (Html.headingStyle Html.h1Font)

  -- h1.app-title *
  select
    ( Select.element "h1"
        `Select.with` Select.byClass h1ClassString
        `Select.deep` Select.star
    )
    (color ∘ oklab ∘ green $ Lightness 0.05)

container :: CSS
container = do
  backgroundColor ∘ oklab ∘ green $ backgroundLightness
  paddingLeft $ rem 4.0
  paddingRight $ rem 4.0
  display flex
  alignItems center
  justifyContent spaceBetween

h1ClassString :: String
h1ClassString = "app-title"

h1Class :: forall i r. Html.ClassProp i r
h1Class = Html.classNames [ h1ClassString ]
