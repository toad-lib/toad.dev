module Kwap.Navbar.Button.Style
  ( selectedClass
  , containerClass
  , textClass
  , global
  ) where

import Kwap.Css
import Prelude hiding (top)

import CSS.Common as Css.Common
import CSS.Cursor as Css.Cursor
import CSS.Selector as Sel
import CSS.Size as Css.Size
import CSS.Time as Time
import CSS.Transform as Css.Transform
import Kwap.Html (ClassProp, classNames, headingStyle)

-- See Kwap.App.Style
global :: CSS
global = do
  selectContainer containerRules
  selectContainerSelected selectedRules
  selectContainerHover containerHoverRules
  selectContainerClick containerClickRules

selectedClass :: String
selectedClass = "navbar-button-selected"

containerClass :: String
containerClass = "navbar-button"

textClass :: String
textClass = "navbar-button-text"

selectContainer :: CSS -> CSS
selectContainer =
  select
    ( Sel.star `Sel.with` (Sel.byClass containerClass)
    )

selectContainerSelected :: CSS -> CSS
selectContainerSelected =
  select
    ( Sel.star `Sel.with` (Sel.byClass selectedClass)
    )

selectContainerClick :: CSS -> CSS
selectContainerClick =
  select
    ( Sel.star `Sel.with`
        ( refinements
            [ Sel.byClass containerClass
            , Sel.pseudo $ "not(." <> selectedClass <> ")"
            , Sel.pseudo "active"
            ]
        )
    )

selectContainerHover :: CSS -> CSS
selectContainerHover =
  select
    ( Sel.star `Sel.with` (refinements [ Sel.byClass containerClass, hover ])
    )

selectedRules :: CSS
selectedRules = do
  containerHoverRules
  select (Sel.star `Sel.with` Sel.byClass textClass) do
    headingStyle $ font' <> fontSize FontSizeH2 <> fontFamily InterExtraBold

font' :: Font
font' = mempty <> fontFamily InterBold <> fontSize FontSizeH3

containerRules :: CSS
containerRules = do
  definedIn "Kwap.Navbar.Button.Style"
  cursor Css.Cursor.Pointer
  display flex
  justifyContent Css.Common.center
  alignItems Css.Common.center
  transition "border" (Time.ms 100.0) kwapEasing (Time.ms 0.0)

  select (Sel.star `Sel.with` Sel.byClass textClass) do
    definedIn "Kwap.Navbar.Button.Style"
    prefixed (Css.Common.browsers <> Plain "user-select") "none"
    headingStyle font'
    transition "font-size" (Time.ms 100.0) kwapEasing (Time.ms 0.0)

containerHoverRules :: CSS
containerHoverRules = do
  definedIn "Kwap.Navbar.Button.Style"

containerClickRules :: CSS
containerClickRules = do
  definedIn "Kwap.Navbar.Button.Style"
  transition "border" (Time.ms 50.0) kwapEasing (Time.ms 0.0)

  select (Sel.star `Sel.with` Sel.byClass textClass) do
    definedIn "Kwap.Navbar.Button.Style"
    transition "font-weight" (Time.ms 50.0) kwapEasing (Time.ms 0.0)
