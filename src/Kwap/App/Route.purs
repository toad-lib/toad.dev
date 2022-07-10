module Kwap.App.Route
  ( Route(..)
  , OneOrAll(..)
  , ConceptPath(..)
  , ofNavbarSection
  , toNavbarSection
  , init
  , codec
  , print
  , conceptPathString
  ) where

import Data.Array (null)
import Data.BooleanAlgebra (not)
import Data.Either (Either)
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Kwap.App.Navbar.Section as App.Navbar
import Prelude (class Eq, class Show, map, ($), (<<<), (>>>))
import Routing.Duplex (RouteDuplex', optional, rest, root)
import Routing.Duplex as Routing.Duplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

newtype ConceptPath = ConceptPath String

derive newtype instance eqConceptPath :: Eq ConceptPath
derive newtype instance showConceptPath :: Show ConceptPath

conceptPathString :: ConceptPath -> String
conceptPathString (ConceptPath s) = s

maybeConceptPath :: RouteDuplex' (Maybe ConceptPath)
maybeConceptPath =
  let
    toSegments = maybe [] (conceptPathString >>> split (Pattern "/"))
    ofSegments = Just
      >>> filter (not <<< null)
      >>> map (joinWith "/" >>> ConceptPath)
  in
    dimap toSegments ofSegments rest

data OneOrAll a = One a | All

derive instance eqOneOrAll :: Eq a => Eq (OneOrAll a)
derive instance genericOneOrAll :: Generic (OneOrAll a) _
instance showOneOrAll :: Show a => Show (OneOrAll a) where
  show = genericShow

maybeOne :: forall a. OneOrAll a -> Maybe a
maybeOne (One a) = Just a
maybeOne All = Nothing

oneMaybe :: forall a. Maybe a -> OneOrAll a
oneMaybe (Just a) = One a
oneMaybe Nothing = All

orAll :: forall a. RouteDuplex' (Maybe a) -> RouteDuplex' (OneOrAll a)
orAll a = dimap maybeOne oneMaybe a

data Route
  = Home
  | Concepts (OneOrAll ConceptPath)
  | Book

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show = genericShow

init :: Route
init = Home

ofNavbarSection :: App.Navbar.Section -> Route
ofNavbarSection App.Navbar.Home = Home
ofNavbarSection App.Navbar.Concepts = Concepts All
ofNavbarSection App.Navbar.Book = Book

toNavbarSection :: Route -> App.Navbar.Section
toNavbarSection Home = App.Navbar.Home
toNavbarSection (Concepts _) = App.Navbar.Concepts
toNavbarSection Book = App.Navbar.Book

print :: Route -> String
print = Routing.Duplex.print codec

codec :: RouteDuplex' Route
codec = root $ sum
  { "Home": noArgs
  , "Concepts": "concepts" / (orAll maybeConceptPath)
  , "Book": "book" / noArgs
  }
