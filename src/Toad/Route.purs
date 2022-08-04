module Toad.Route
  ( Route(..)
  , OneOrAll(..)
  , ofNavbarSection
  , toNavbarSection
  , init
  , codec
  , print
  , maybeOne
  ) where

import Toad.Prelude hiding ((/))

import Data.Array (null)
import Data.Filterable (filter)
import Data.Hashable (class Hashable, hash)
import Data.Profunctor (dimap)
import Data.String (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Routing.Duplex (RouteDuplex', rest, root)
import Routing.Duplex as Routing.Duplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Toad.Concept as Concept
import Toad.Navbar.Section as Navbar

maybeConceptIdent :: RouteDuplex' (Maybe Concept.Ident)
maybeConceptIdent =
  let
    toSegments = maybe [] (Concept.identString >>> split (Pattern "/"))
    ofSegments = Just
      >>> filter (not <<< null)
      >>> map (joinWith "/" >>> Concept.Ident)
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
  | Concepts (OneOrAll Concept.Ident)
  | Book

instance hashRoute :: Hashable Route where
  hash Home = 100
  hash (Concepts (One ident)) = hash ident
  hash (Concepts All) = 101
  hash Book = 102

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show = genericShow

init :: Route
init = Home

ofNavbarSection :: Navbar.Section -> Route
ofNavbarSection Navbar.Home = Home
ofNavbarSection Navbar.Concepts = Concepts All
ofNavbarSection Navbar.Book = Book

toNavbarSection :: Route -> Navbar.Section
toNavbarSection Home = Navbar.Home
toNavbarSection (Concepts _) = Navbar.Concepts
toNavbarSection Book = Navbar.Book

print :: Route -> String
print = Routing.Duplex.print codec

codec :: RouteDuplex' Route
codec = root $ sum
  { "Home": noArgs
  , "Concepts": "concepts" / (orAll maybeConceptIdent)
  , "Book": "book" / noArgs
  }
