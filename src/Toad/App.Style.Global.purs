module Toad.Style.Global (stylesheet) where

import Prelude

import Halogen.HTML (HTML)
import Halogen.HTML.CSS as HalogenCss
import Toad.Css.Font.Style as Font.Style
import Toad.Navbar.Button.Style as Navbar.Button.Style

-- Here lie global styles applied to a <style> element
-- This is where all selectors /other than/ inline styles live, i.e. :hover selectors.
--
-- This should be used as little as possible - within reason.
--
-- If your goal is to create shared styles, prefer a commonly-accessible
-- purescript CSS expression over sharing classes between elements.
--
-- Do not define styles in this module, instead aggregate styles from
-- their most specific locations.
--
-- **All** CSS selections must use the Toad.Css.definedIn rule, so that
-- the rendered global CSS provides breadcrumbs back to the implementation.

stylesheet :: forall w i. HTML w i
stylesheet = HalogenCss.stylesheet do
  Font.Style.global
  Navbar.Button.Style.global
