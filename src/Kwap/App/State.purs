module Kwap.App.State where

import Prelude

import Control.Alt ((<|>))
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Kwap.App.Css (KwapGradient, kwapGradientInit)
import Kwap.App.Navbar as App.Navbar
import Kwap.Concept as Concept

newtype ErrorMessage = ErrorMessage String

data State = State (Maybe ErrorMessage) (Maybe App.Navbar.Section) (Maybe KwapGradient)
  (Maybe Concept.Decl) 

class LiftState a where
  liftState :: a -> State

instance conceptDeclAppState :: LiftState (Either String Concept.Decl) where
  liftState (Right d) = State Nothing Nothing Nothing (Just d)
  liftState (Left m) = State (Just $ ErrorMessage m) Nothing Nothing Nothing

instance kwapGradientAppState :: LiftState KwapGradient where
  liftState g = State Nothing Nothing (Just g) Nothing

instance navbarSecAppState :: LiftState App.Navbar.Section where
  liftState n = State Nothing (Just n) Nothing Nothing

error :: State -> Maybe String
error (State (Just (ErrorMessage e)) _ _ _) = Just e
error (State (Nothing) _ _ _) = Nothing

navbarSection :: State -> App.Navbar.Section
navbarSection (State _ n _ _) = fromMaybe App.Navbar.Home n

kwapGradient :: State -> KwapGradient
kwapGradient (State _ _ g _) = fromMaybe kwapGradientInit g

conceptDecls :: State -> Maybe Concept.Decl
conceptDecls (State _ _ _ d) = d

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append (State eA nA gA cdA) (State eB nB gB cdB) =
    let
      err = eA <|> eB
      nav = nA <|> nB
      grad = gA <|> gB
      concepts = cdA <|> cdB
    in
      State err nav grad concepts

instance monoidState :: Monoid State where
  mempty = State Nothing Nothing Nothing Nothing
