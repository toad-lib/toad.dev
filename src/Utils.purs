module Utils where

import Prelude
import Data.Foldable (class Foldable)
import Halogen.HTML.Properties as Props
import Halogen.HTML.Core (ClassName(..))

classes :: ∀ r i. Array String -> Props.IProp ( class :: String | r ) i
classes = map ClassName >>> Props.classes

appendFoldable :: ∀ f s a. Foldable f => Semigroup (s a) => (f ~> s) -> s a -> f a -> s a
appendFoldable fromFoldable = flip $ fromFoldable >>> append

test :: ∀ f a. Applicative f => Monoid (f a) => Boolean -> a -> f a
test true = pure

test false = const mempty
