module Toad.Atom.Logo (render) where

import Prelude hiding (bottom, top)

import CSS.Common as Css.Common
import CSS.Size as Css.Size
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties as HP
import Toad.Atom.Logo.Style (kwapMaskUrl, logoContainer)
import Toad.Css as Css
import Toad.Html as HH

render :: ∀ w i. Maybe Css.CSS -> HH.HTML w i
render extra = HH.div
  [ Css.style do
      logoContainer
      foldl (const identity) (pure unit) extra
  ]
  [ HH.img
      [ Css.style do
          Css.height $ Css.pct 100.0
      , HP.src kwapMaskUrl
      ]
  ]
