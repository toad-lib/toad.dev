module Color where

import Prelude
import Halogen.HTML.Core (ClassName(..))

data Accent = Blue
            | Yellow
            | Green

instance showAccent :: Show Accent where
  show Blue   = "blue"
  show Yellow = "yellow"
  show Green  = "green"

bgClass :: Accent -> ClassName
bgClass color = ClassName $ "bg-" <> show color

ribbonClass :: Accent -> ClassName
ribbonClass color = ClassName $ "ribbon-" <> show color
