module Kwap.App.Navbar.Button (render, State(..)) where

import Prelude

import Data.Array (catMaybes)
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Navbar.Button.Style as Style

data State
  = Selected
  | NotSelected

isSelected :: State -> Boolean
isSelected Selected = true
isSelected _ = false

render :: ∀ a w. a -> State -> String -> Css.CSS -> HH.HTML w a
render a state text extra = HH.div
  [ Css.style extra
  , HH.classNames $ catMaybes
      [ pure Style.containerClass
      , const Style.selectedClass <$> filter isSelected (pure state)
      ]
  , HE.onClick (\_ -> a)
  ]
  [ HH.withText (flip HH.h1) text [ HH.classNames [ Style.textClass ] ]
  ]
