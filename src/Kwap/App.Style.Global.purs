module Kwap.App.Style.Global (stylesheet) where

import Kwap.App.Css
import Prelude

import Halogen.HTML (HTML)
import Halogen.HTML.CSS as HalogenCss
import Kwap.App.Navbar.Button.Style as Navbar.Button.Style

-- Here lie global styles applied to a <style> element
-- This is where all selectors /other than/ inline styles live, i.e. :hover selectors.
--
-- Nothing should be defined directly in this module and should
-- instead aggregate the styles from local style modules.
--
-- In addition, this should be used as little as reasonable, and if
-- your goal is to create shared styles, prefer a commonly-accessible
-- purescript CSS expression over sharing classes between elements.

stylesheet :: forall w i. HTML w i
stylesheet = HalogenCss.stylesheet do
  Navbar.Button.Style.global
