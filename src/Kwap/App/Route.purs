module Kwap.App.Route
  ( Route(..)
  , ofNavbarSection
  , toNavbarSection
  , init
  , parse
  , print
  ) where

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Kwap.App.Navbar as App.Navbar
import Prelude (class Eq, class Show, ($))
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex as Routing.Duplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

newtype ConceptPath = ConceptPath String

data Route
  = Home
  | Concepts
  | Book

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show = genericShow

init :: Route
init = Home

ofNavbarSection :: App.Navbar.Section -> Route
ofNavbarSection App.Navbar.Home = Home
ofNavbarSection App.Navbar.Concepts = Concepts
ofNavbarSection App.Navbar.Book = Book

toNavbarSection :: Route -> App.Navbar.Section
toNavbarSection Home = App.Navbar.Home
toNavbarSection Concepts = App.Navbar.Concepts
toNavbarSection Book = App.Navbar.Book

parse :: String -> Either RouteError Route
parse = Routing.Duplex.parse codec

print :: Route -> String
print = Routing.Duplex.print codec

codec :: RouteDuplex' Route
codec = root $ sum
  { "Home": noArgs
  , "Concepts": "concepts" / noArgs
  , "Book": "book" / noArgs
  }
