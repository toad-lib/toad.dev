module Toad.State
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
  , NavAccordions(..)
  , navAccordions
  , navAccordionsToggleBook
  , navAccordionsToggleConcepts
  , navAccordionsBookExpanded
  , navAccordionsConceptsExpanded
  ) where

import Toad.Prelude

import Control.Alt ((<|>))
import Data.Expanded (Expanded(..))
import Data.Expanded as Expanded
import Data.Foldable (find)
import Data.Hashable (hash)
import Data.Array as Array
import Data.List (List)
import Data.Map (Map, SemigroupMap, keys)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Toad.Concept as Concept
import Toad.Error (ErrorMessage(..))
import Toad.Markdown as Md
import Toad.Route as Route

newtype NavAccordions = NavAccordions { book :: Expanded, concepts :: Expanded }

derive instance genericNavAccordions :: Generic NavAccordions _
derive instance eqNavAccordions :: Eq NavAccordions
instance showNavAccordions :: Show NavAccordions where
  show = genericShow

navAccordionsInit :: Maybe Route.Route -> NavAccordions
navAccordionsInit (Just Route.Book) = NavAccordions { book: Expanded, concepts: Collapsed }
navAccordionsInit (Just (Route.Concepts _)) = NavAccordions { book: Collapsed, concepts: Expanded }
navAccordionsInit _ = NavAccordions { book: Collapsed, concepts: Collapsed }

navAccordionsBookExpanded :: NavAccordions -> Expanded
navAccordionsBookExpanded (NavAccordions { book }) = book

navAccordionsConceptsExpanded :: NavAccordions -> Expanded
navAccordionsConceptsExpanded (NavAccordions { concepts: c }) = c

navAccordionsToggleBook :: NavAccordions -> NavAccordions
navAccordionsToggleBook (NavAccordions { book, concepts: c }) = NavAccordions
  { book: Expanded.toggle book, concepts: c }

navAccordionsToggleConcepts :: NavAccordions -> NavAccordions
navAccordionsToggleConcepts (NavAccordions { book, concepts: c }) =
  NavAccordions { book, concepts: Expanded.toggle c }

data State = State
  (Maybe ErrorMessage)
  (Maybe Concept.Manifest)
  (Maybe Route.Route)
  (SemigroupMap Concept.Ident Md.Document)
  (Maybe NavAccordions)

derive instance eqState :: Eq State
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append (State eA cdA rA dsA nA) (State eB cdB rB dsB nB) =
    let
      err = eA <|> eB
      concepts_ = cdA <|> cdB
      route_ = rA <|> rB
      docs = dsA <|> dsB
      n = nA <|> nB
    in
      State err concepts_ route_ docs n

instance monoidState :: Monoid State where
  mempty = State Nothing Nothing Nothing mempty Nothing

init :: State
init = mempty

dismissError :: State -> State
dismissError (State _ a b c d) = State Nothing a b c d

error :: State -> Maybe String
error (State (Just (ErrorMessage e)) _ _ _ _) = Just e
error (State (Nothing) _ _ _ _) = Nothing

conceptManifest :: State -> Maybe Concept.Manifest
conceptManifest (State _ m _ _ _) = m

conceptManifestHash :: State -> Int
conceptManifestHash (State _ m _ _ _) =
  hash ∘ map Concept.ident ∘ maybe [] Concept.decls $ m

lookupDecl :: Concept.Ident -> State -> Maybe Concept.Decl
lookupDecl ident s = do
  ds <- Concept.decls <$> conceptManifest s
  find (eq ident ∘ Concept.ident) ds

route :: State -> Route.Route
route (State _ _ r _ _) = fromMaybe Route.init r

routeHash :: State -> Int
routeHash = hash ∘ route

concepts :: State -> Map Concept.Ident Md.Document
concepts (State _ _ _ m _) = unwrap m

conceptsHash :: State -> Int
conceptsHash = hash ∘ (Set.toUnfoldable :: Set _ -> List _) ∘ keys ∘ concepts

navAccordions :: State -> NavAccordions
navAccordions (State _ _ _ _ n) = maybe (navAccordionsInit Nothing) id n

class LiftState a where
  liftState :: a -> State

instance errorMessageAppState :: LiftState ErrorMessage where
  liftState e = State (Just e) Nothing Nothing mempty Nothing

instance conceptManifestAppState :: LiftState Concept.Manifest where
  liftState (Concept.Manifest ds) = State Nothing (Just ∘ Concept.Manifest ∘ Array.sortWith Concept.title $ ds) Nothing mempty Nothing

instance routeAppState :: LiftState Route.Route where
  liftState r = State Nothing Nothing (Just r) mempty (Just $ navAccordionsInit $ Just r)

instance conceptsAppState :: LiftState (Map Concept.Ident Md.Document) where
  liftState ds = State Nothing Nothing Nothing (wrap ds) Nothing

instance navAccordionsAppState :: LiftState NavAccordions where
  liftState n = State Nothing Nothing Nothing mempty (Just n)
