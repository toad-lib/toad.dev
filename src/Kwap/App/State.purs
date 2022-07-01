module Kwap.App.State where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Kwap.App.Css (KwapGradient, kwapGradientInit)
import Kwap.App.Navbar as App.Navbar

class LiftState a where
  liftState :: a -> State

instance kwapGradientAppState :: LiftState KwapGradient where
  liftState g = State Nothing (Just g)

instance navbarSecAppState :: LiftState App.Navbar.Section where
  liftState n = State (Just n) Nothing

data State = State (Maybe App.Navbar.Section) (Maybe KwapGradient)

navbarSection :: State -> App.Navbar.Section
navbarSection (State n _) = fromMaybe App.Navbar.Home n

kwapGradient :: State -> KwapGradient
kwapGradient (State _ g) = fromMaybe kwapGradientInit g

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append (State nA gA) (State nB gB) =
    let
      nav = nA <|> nB
      grad = gA <|> gB
    in
      State nav grad

instance monoidState :: Monoid State where
  mempty = State Nothing Nothing
