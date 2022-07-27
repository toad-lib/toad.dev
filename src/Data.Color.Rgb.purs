module Data.Color.Rgb where

import Prelude

import CSS as Css
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype R = R Number

derive instance genericR :: Generic R _
instance showR :: Show R where
  show = genericShow

newtype G = G Number

derive instance genericG :: Generic G _
instance showG :: Show G where
  show = genericShow

newtype B = B Number

derive instance genericB :: Generic B _
instance showB :: Show B where
  show = genericShow

data Rgb = Rgb R G B

derive instance genericRgb :: Generic Rgb _
instance showRgb :: Show Rgb where
  show = genericShow

-- css :: Rgb -> Css.Value
-- css (Rgb (R r) (G g) (B b)) = ga
