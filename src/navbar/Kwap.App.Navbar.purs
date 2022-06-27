module Kwap.App.Navbar where

import Prelude

import Kwap.App.Css as Css
import CSS.Common as Css.Common
import CSS.Size as Css.Size
import Data.Fist
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

data NavbarGridRegion = Logo
                      | GetStarted
                      | Docs
                      | Github

-- instance navbarGrid :: GridRows NavbarGridRegion Fist2 where
--   rows = [ Row (Css.Common.other $ Css.value $ Css.px 1.0) (fist2 Logo GetStarted)
--          , Row (Css.Common.other $ Css.value $ Css.px 1.0) (fist2 GetStarted)
--          , Row (Css.Common.other $ Css.value $ Css.px 1.0) (fist2 Docs)
--          , Row (Css.Common.other $ Css.value $ Css.px 1.0) (fist2 Github)
--          ]

-- grid :: AppLayout -> Css.CSS
-- grid layout =
  -- let
    -- appAreaTemplate area' =
      -- [ Css.fromString $ "[" <> (gridAreaLabel area') <> "]"
      -- , Css.value (gridAreaSize area')
      -- ] # Css.noCommas
    -- appTemplate = gridAreas <#> appAreaTemplate # Css.noCommas
-- 
    -- --| This is a single column/row that occupies the full width/height of the viewport
    -- justOne =
      -- [ Css.fromString
          -- $ "["
          -- <> (gridAreas <#> gridAreaLabel # String.joinWith " ")
          -- <> "]"
      -- , Css.value (Css.pct 100.0)
      -- ] # Css.noCommas
-- 
    -- rows = case layout of
             -- AppLayoutDesktop -> justOne
             -- AppLayoutMobile -> appTemplate
-- 
    -- cols = case layout of
             -- AppLayoutDesktop -> appTemplate
             -- AppLayoutMobile -> justOne
  -- in
    -- do
      -- Css.display Css.grid
      -- Css.key (Css.fromString "grid-template-rows") rows
      -- Css.key (Css.fromString "grid-template-columns") cols
