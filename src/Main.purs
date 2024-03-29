module Main where

import Toad.Prelude

import Control.Monad.Rec.Class (forever)
import Data.Bifunctor (lmap, rmap)
import Data.Either (blush, note)
import Data.Expanded as Expanded
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Fetch.Browser (windowFetch)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Parsing (runParser)
import Routing.Duplex as Routing.Duplex
import Routing.Hash as Routing.Hash
import Toad.Action as Toad.Action
import Toad.App as Toad
import Toad.Atom.AppTitle (AppTitle(..))
import Toad.Concept as Concept
import Toad.Concept.Fetch as Concept.Fetch
import Toad.Error as Toad.Error
import Toad.Markdown as Md
import Toad.Navigate (navigate)
import Toad.Page.Concepts as Toad.Page.Concepts
import Toad.Query as Toad.Query
import Toad.Route as Toad.Route
import Toad.State as Toad.State

left :: forall a b. Either a b -> Maybe a
left = blush

main :: Effect Unit
main =
  let
    tellAppRouteChanged _ (Just prev) new | prev == new = pure unit
    tellAppRouteChanged io _ route =
      void
        ∘ Aff.launchAff
        ∘ Toad.Query.sendNavigate (fromMaybe Toad.Route.Home route)
        $ io
  in
    HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI (H.hoist Toad.runM component) unit body
      H.liftEffect ∘ void
        ∘ Routing.Hash.matchesWith
            (Routing.Duplex.parse ∘ Routing.Duplex.optional $ Toad.Route.codec)
        $
          tellAppRouteChanged io

component :: ∀ i. H.Component Toad.Query.Query i Unit Toad.M
component =
  H.mkComponent
    { initialState: const Toad.State.init
    , render: Toad.render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Toad.Action.Init
        }
    }

timer :: ∀ m a. MonadAff m => Milliseconds -> a -> m (HS.Emitter a)
timer ms val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff ∘ Aff.forkAff ∘ forever $ do
    Aff.delay ms
    H.liftEffect $ HS.notify listener val
  pure emitter

handleQuery
  :: ∀ a
   . Toad.Query.Query a
  -> H.HalogenM Toad.State.State Toad.Action.Action Toad.Slots Unit Toad.M
       (Maybe a)
handleQuery = case _ of
  Toad.Query.Navigate route _ -> do
    Toad.put route
    pure Nothing

handleAction
  :: ∀ s o
   . Toad.Action.Action
  -> H.HalogenM Toad.State.State Toad.Action.Action s o Toad.M Unit
handleAction =
  case _ of
    Toad.Action.Init -> do
      -- _ <- H.subscribe =<< timer (Milliseconds 100.0) Toad.Action.Tick

      conceptManifest <- H.liftAff $ Concept.Fetch.manifest windowFetch

      fromMaybe (pure unit)
        ∘ map H.liftEffect
        ∘ map Console.error
        ∘ left
        $ conceptManifest

      Toad.handleError
        ∘ lmap Toad.Error.fetchingManifest
        ∘ rmap Toad.put
        $ conceptManifest

    Toad.Action.NavbarSectionPicked -> mempty

    Toad.Action.AppTitleChanged at -> Toad.put at

    Toad.Action.Navigate r -> navigate r

    Toad.Action.Tick -> mempty

    Toad.Action.DismissError -> H.put =<< H.modify Toad.State.dismissError

    Toad.Action.Nop -> mempty

    Toad.Action.NavAccordionExpandBook -> flip bind Toad.put
      ∘ map Toad.State.navAccordionsToggleBook
      ∘ map Toad.State.navAccordions
      $ H.get

    Toad.Action.NavAccordionExpandConcepts -> flip bind Toad.put
      ∘ map Toad.State.navAccordionsToggleConcepts
      ∘ map Toad.State.navAccordions
      $ H.get

    Toad.Action.ConceptsPageOutput (Toad.Page.Concepts.TitleChanged at) ->
      handleAction (Toad.Action.AppTitleChanged at)

    Toad.Action.ConceptsPageOutput (Toad.Page.Concepts.FetchConcept ident) -> do
      let
        fetchConcept =
          H.liftAff ∘ Concept.Fetch.one windowFetch ∘ Concept.path

      let
        tryParseConcept =
          flip bind
            $ lmap Toad.Error.parsingConcept
            ∘ lmap show
            ∘ (flip runParser) Md.documentP

      let storeDoc s d = Map.insert ident d ∘ Toad.State.concepts $ s

      let
        parseDocAndPut mdM = do
          state <- H.get
          md <- mdM
          Toad.handleError
            ∘ rmap (Toad.put ∘ storeDoc state)
            ∘ tryParseConcept
            $ md

      bind H.get
        $ parseDocAndPut
        ∘ traverse fetchConcept
        ∘ lmap Toad.Error.lookingUpRouteConcept
        ∘ note ("concept " <> show ident <> " not found")
        ∘ Toad.State.lookupDecl ident
