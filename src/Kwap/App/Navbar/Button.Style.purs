module Kwap.App.Navbar.Button.Style (selectedClass, containerClass, textClass, global) where

import Kwap.App.Css
import Prelude hiding (top)

import CSS.Common as Css.Common
import CSS.Cursor as Css.Cursor
import CSS.Selector as Sel
import CSS.Size as Css.Size
import CSS.Time as Time
import CSS.Transform as Css.Transform
import Kwap.App.Html (classNames, ClassProp, headingStyle)

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
    (Sel.star `Sel.with` (Sel.byClass selectedClass)
    )

selectContainerClick :: CSS -> CSS
selectContainerClick =
  select
    (Sel.star `Sel.with` (refinements [ Sel.byClass containerClass, Sel.pseudo $ "not(."<>selectedClass<>")", Sel.pseudo "active" ])
    )

selectContainerHover :: CSS -> CSS
selectContainerHover =
    select
      (Sel.star `Sel.with` (refinements [ Sel.byClass containerClass, hover ])
      )

selectedRules :: CSS
selectedRules = do
  containerHoverRules
  select (Sel.star `Sel.with` Sel.byClass textClass) do
    headingStyle $ font' <> fontSize FontSizeH1 <> fontFamily InterExtraBold
    color $ Yellow Lightest

font' :: Font
font' = mempty <> fontFamily InterBold <> fontSize FontSizeH2

containerRules :: CSS
containerRules = do
  border solid (rem 1.0) (cssColor $ Yellow Lightest)
  cursor Css.Cursor.Pointer
  display flex
  justifyContent Css.Common.center
  alignItems Css.Common.center
  transition "border" (Time.ms 100.0) kwapEasing (Time.ms 0.0)

  select (Sel.star `Sel.with` Sel.byClass textClass) do
    prefixed (Css.Common.browsers <> Plain "user-select") "none"
    headingStyle font'
    color $ Yellow Lightest
    transition "font-size" (Time.ms 100.0) kwapEasing (Time.ms 0.0)

containerHoverRules :: CSS
containerHoverRules = do
  border solid (rem 0.25) (cssColor $ Yellow Lightest)

containerClickRules :: CSS
containerClickRules = do
  transition "border" (Time.ms 50.0) kwapEasing (Time.ms 0.0)
  border solid (rem 0.5) (cssColor $ Yellow Lightest)

  select (Sel.star `Sel.with` Sel.byClass textClass) do
    transition "font-weight" (Time.ms 50.0) kwapEasing (Time.ms 0.0)
