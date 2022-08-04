module Toad.Navbar.Button (render, State(..)) where

import Toad.Prelude

import Data.Array (catMaybes)
import Data.Filterable (filter)
import Halogen.HTML.Events as HE
import Toad.Css as Css
import Toad.Html as HH
import Toad.Navbar.Button.Style as Style

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
  [ HH.withText (flip HH.h2) text [ HH.classNames [ Style.textClass ] ]
  ]
