module Kwap.State
  ( State(..)
  , ErrorMessage(..)
  , init
  , class LiftState
  , liftState
  , error
  , conceptDecl
  , navbarSection
  , kwapGradient
  , route
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Kwap.Concept as Concept
import Kwap.Css (KwapGradient, kwapGradientInit)
import Kwap.Navbar.Section as Navbar
import Kwap.Route as Route

newtype ErrorMessage = ErrorMessage String

derive newtype instance showErrorMessage :: Show ErrorMessage
derive newtype instance eqErrorMessage :: Eq ErrorMessage

data State = State (Maybe ErrorMessage)
  (Maybe KwapGradient)
  (Maybe Concept.Decl)
  (Maybe Route.Route)

derive instance eqState :: Eq State
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append (State eA gA cdA rA) (State eB gB cdB rB) =
    let
      err = eA <|> eB
      grad = gA <|> gB
      concepts = cdA <|> cdB
      route_ = rA <|> rB
    in
      State err grad concepts route_

instance monoidState :: Monoid State where
  mempty = State Nothing Nothing Nothing Nothing

init :: State
init = mempty

error :: State -> Maybe String
error (State (Just (ErrorMessage e)) _ _ _) = Just e
error (State (Nothing) _ _ _) = Nothing

navbarSection :: State -> Navbar.Section
navbarSection s = Route.toNavbarSection <<< route $ s

kwapGradient :: State -> KwapGradient
kwapGradient (State _ g _ _) = fromMaybe kwapGradientInit g

conceptDecl :: State -> Maybe Concept.Decl
conceptDecl (State _ _ d _) = d

route :: State -> Route.Route
route (State _ _ _ r) = fromMaybe Route.init r

class LiftState a where
  liftState :: a -> State

instance routeAppState :: LiftState Route.Route where
  liftState r = State Nothing Nothing Nothing (Just r)

instance conceptDeclAppState :: LiftState (Either String Concept.Decl) where
  liftState (Right d) = State Nothing Nothing (Just d) Nothing
  liftState (Left m) = State (Just $ ErrorMessage m) Nothing Nothing Nothing

instance kwapGradientAppState :: LiftState KwapGradient where
  liftState g = State Nothing (Just g) Nothing Nothing
