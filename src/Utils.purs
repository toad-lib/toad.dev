module Utils where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties as Props
import Halogen.HTML.Core (ClassName(..))

classes :: ∀ r i. Array String -> Props.IProp ( class :: String | r ) i
classes = map ClassName >>> Props.classes

appendFoldable :: ∀ f a. Foldable f => Array a -> f a -> Array a
appendFoldable = flip $ fromFoldable >>> append

test :: ∀ a. Boolean -> a -> Maybe a
test true a = Just a

test false a = Nothing
