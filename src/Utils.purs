module Utils where

import Prelude
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties as Props
import Halogen.HTML.Core (ClassName(..))

classes :: forall r i. Array String -> Props.IProp (class :: String | r) i
classes strs = Props.classes $ strs <#> ClassName

maybeArray :: forall a. Maybe a -> Array a
maybeArray (Just a) = [a]
maybeArray Nothing  = []

snocMaybe :: forall a. Array a -> Maybe a -> Array a
snocMaybe arr (Just a) = snoc arr a
snocMaybe arr Nothing  = arr

test :: forall a. Boolean -> a -> Maybe a
test bool a | bool == true = Just a
            | otherwise    = Nothing
