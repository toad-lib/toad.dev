module Kwap.App.Css
  ( module X
  , style
  ) where

import Prelude

import CSS as Css
import CSS hiding
  ( Color(..)
  , FontStyle(..)
  , FontWeight(..)
  , backgroundColor
  , color
  , fontFamily
  , fontSize
  , fontWeight
  ) as X
import CSS.Render as Css.Render
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong (second)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties as HP
import Kwap.App.Css.Color as X
import Kwap.App.Css.Font as X

--| copied from purescript-halogen-css, but modified to
--| preserve duplicate rules for vendoring
style ∷ ∀ i r. Css.CSS -> HP.IProp (style :: String | r) i
style =
  HP.attr (HC.AttrName "style")
    <<< toString
    <<< rules
    <<< Css.runS
  where
  toString :: Array (Tuple String String) -> String
  toString = String.joinWith "; " <<< map
    (\(Tuple key val) -> key <> ": " <> val)

  rules :: Array Css.Rule -> Array (Tuple String String)
  rules rs = Array.mapMaybe property rs
    >>= Css.Render.collect
      >>> rights

  property :: Css.Rule -> Maybe (Tuple (Css.Key Unit) Css.Value)
  property (Css.Property k v) = Just (Tuple k v)
  property _ = Nothing

  rights :: ∀ a b. Array (Either a b) -> Array b
  rights = Array.concatMap $ foldMap Array.singleton
