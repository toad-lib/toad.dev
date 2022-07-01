module Kwap.App.Style (global) where

import Prelude

import Kwap.App.Css
import Halogen.HTML (HTML)
import Halogen.HTML.CSS (stylesheet)
import Kwap.App.Navbar.Button.Style as Navbar.Button.Style

-- Here lie global styles applied to a <style> element
-- This is where all selectors /other than/ inline styles live, i.e. :hover selectors.
--
-- Nothing should be defined directly in this module and should
-- instead aggregate the styles from local style modules.

global :: forall w i. HTML w i
global = stylesheet do
  Navbar.Button.Style.global
