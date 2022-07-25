module Kwap.State
  ( State(..)
  , init
  , concepts
  , conceptsHash
  , class LiftState
  , dismissError
  , liftState
  , lookupDecl
  , error
  , conceptManifest
  , conceptManifestHash
  , route
  , routeHash
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Hashable (hash)
import Data.List (List)
import Data.Map (Map, SemigroupMap, keys)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Kwap.Concept as Concept
import Kwap.Error (ErrorMessage(..))
import Kwap.Markdown as Md
import Kwap.Route as Route

data State = State
  (Maybe ErrorMessage)
  (Maybe Concept.Manifest)
  (Maybe Route.Route)
  (SemigroupMap Concept.Ident Md.Document)

derive instance eqState :: Eq State
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append (State eA cdA rA dsA) (State eB cdB rB dsB) =
    let
      err = eA <|> eB
      concepts_ = cdA <|> cdB
      route_ = rA <|> rB
      docs = dsA <|> dsB
    in
      State err concepts_ route_ docs

instance monoidState :: Monoid State where
  mempty = State Nothing Nothing Nothing mempty

init :: State
init = mempty

dismissError :: State -> State
dismissError (State _ a b c) = State Nothing a b c

error :: State -> Maybe String
error (State (Just (ErrorMessage e)) _ _ _) = Just e
error (State (Nothing) _ _ _) = Nothing

conceptManifest :: State -> Maybe Concept.Manifest
conceptManifest (State _ m _ _) = m

conceptManifestHash :: State -> Int
conceptManifestHash (State _ m _ _) =
  hash <<< map Concept.ident <<< maybe [] Concept.decls $ m

lookupDecl :: Concept.Ident -> State -> Maybe Concept.Decl
lookupDecl ident s = do
  ds <- Concept.decls <$> conceptManifest s
  find (eq ident <<< Concept.ident) ds

route :: State -> Route.Route
route (State _ _ r _) = fromMaybe Route.init r

routeHash :: State -> Int
routeHash = hash <<< route

concepts :: State -> Map Concept.Ident Md.Document
concepts (State _ _ _ m) = unwrap m

conceptsHash :: State -> Int
conceptsHash = hash <<< (Set.toUnfoldable :: Set _ -> List _) <<< keys <<<
  concepts

class LiftState a where
  liftState :: a -> State

instance errorMessageAppState :: LiftState ErrorMessage where
  liftState e = State (Just e) Nothing Nothing mempty

instance conceptManifestAppState :: LiftState Concept.Manifest where
  liftState m = State Nothing (Just m) Nothing mempty

instance routeAppState :: LiftState Route.Route where
  liftState r = State Nothing Nothing (Just r) mempty

instance conceptsAppState :: LiftState (Map Concept.Ident Md.Document) where
  liftState ds = State Nothing Nothing Nothing (wrap ds)
