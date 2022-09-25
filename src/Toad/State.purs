module Toad.State
  ( State
  , Record
  , state
  , record
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
import Data.Array as Array
import Data.Expanded (Expanded(..))
import Data.Expanded as Expanded
import Data.Foldable (find)
import Data.Hashable (hash)
import Data.List (List)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Toad.Atom.AppTitle (AppTitle(..))
import Toad.Concept as Concept
import Toad.Error (ErrorMessage(..), errorMessageString)
import Toad.Markdown as Md
import Toad.Route as Route

newtype NavAccordions = NavAccordions { book :: Expanded, concepts :: Expanded }

derive instance genericNavAccordions :: Generic NavAccordions _
derive instance eqNavAccordions :: Eq NavAccordions
instance showNavAccordions :: Show NavAccordions where
  show = genericShow

navAccordionsInit :: Maybe Route.Route -> NavAccordions
navAccordionsInit (Just Route.Book) = NavAccordions
  { book: Expanded, concepts: Collapsed }
navAccordionsInit (Just (Route.Concepts _)) = NavAccordions
  { book: Collapsed, concepts: Expanded }
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

type Record =
  { error :: Maybe ErrorMessage
  , manifest :: Maybe Concept.Manifest
  , route :: Maybe Route.Route
  , parsedConcepts :: SemigroupMap Concept.Ident Md.Document
  , navAccordions :: Maybe NavAccordions
  , appTitle :: Maybe AppTitle
  }

data State = State Record

derive instance eqState :: Eq State
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow

--| Old state should always be on the right
instance semiState :: Semigroup State where
  append
    ( State
        { error: eA
        , manifest: mA
        , route: rA
        , parsedConcepts: pcA
        , navAccordions: nA
        , appTitle: atA
        }
    )
    ( State
        { error: eB
        , manifest: mB
        , route: rB
        , parsedConcepts: pcB
        , navAccordions: nB
        , appTitle: atB
        }
    ) =
    let
      error_ = eA <|> eB
      manifest_ = mA <|> mB
      route_ = rA <|> rB
      parsedConcepts_ = pcA <|> pcB
      navAccordions_ = nA <|> nB
      appTitle_ = atA <|> atB
    in
      State
        { error: error_
        , manifest: manifest_
        , route: route_
        , parsedConcepts: parsedConcepts_
        , navAccordions: navAccordions_
        , appTitle: appTitle_
        }

instance monoidState :: Monoid State where
  mempty = State
    { error: Nothing
    , manifest: Nothing
    , route: Nothing
    , parsedConcepts: SemigroupMap $ Map.empty
    , navAccordions: Nothing
    , appTitle: Nothing
    }

init :: State
init = mempty

initWith :: (Record -> Record) -> State
initWith f = state ∘ f ∘ record $ init

record :: State -> Record
record (State r) = r

state :: Record -> State
state = State

dismissError :: State -> State
dismissError = state ∘ (_ { error = Nothing }) ∘ record

error :: State -> Maybe String
error = map errorMessageString ∘ (_.error) ∘ record

conceptManifest :: State -> Maybe Concept.Manifest
conceptManifest = _.manifest ∘ record

conceptManifestHash :: State -> Int
conceptManifestHash =
  hash
    ∘ map Concept.ident
    ∘ maybe [] Concept.decls
    ∘ conceptManifest

lookupDecl :: Concept.Ident -> State -> Maybe Concept.Decl
lookupDecl ident s = do
  ds <- Concept.decls <$> conceptManifest s
  find (eq ident ∘ Concept.ident) ds

route :: State -> Route.Route
route = fromMaybe Route.init ∘ (_.route) ∘ record

routeHash :: State -> Int
routeHash = hash ∘ route

concepts :: State -> Map Concept.Ident Md.Document
concepts = unwrap ∘ (_.parsedConcepts) ∘ record

conceptsHash :: State -> Int
conceptsHash = hash ∘ (Set.toUnfoldable :: Set _ -> List _) ∘ Map.keys ∘
  concepts

navAccordions :: State -> NavAccordions
navAccordions = maybe (navAccordionsInit Nothing) id ∘ (_.navAccordions) ∘
  record

class LiftState a where
  liftState :: a -> State

instance errorMessageAppState :: LiftState ErrorMessage where
  liftState e = initWith (_ { error = Just e })

instance conceptManifestAppState :: LiftState Concept.Manifest where
  liftState (Concept.Manifest ds) =
    initWith
      ( _
          { manifest =
              (Just ∘ Concept.Manifest ∘ Array.sortWith Concept.title $ ds)
          }
      )

instance routeAppState :: LiftState Route.Route where
  liftState r = initWith
    (_ { route = Just r, navAccordions = Just (navAccordionsInit $ Just r) })

instance conceptsAppState :: LiftState (Map Concept.Ident Md.Document) where
  liftState ds = initWith (_ { parsedConcepts = wrap ds })

instance navAccordionsAppState :: LiftState NavAccordions where
  liftState n = initWith (_ { navAccordions = Just n })

instance appTitleLiftState :: LiftState AppTitle where
  liftState at = initWith (_ { appTitle = Just at })
