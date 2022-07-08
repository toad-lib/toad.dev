module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Kwap.App as App
import Kwap.App.Action as App.Action
import Kwap.App.Css as App.Css
import Kwap.App.Query as App.Query
import Kwap.App.Route as App.Route
import Kwap.App.State as App.State
import Kwap.Concept as Concept
import Kwap.Navigate (navigate)
import Routing.Hash as Routing.Hash

main :: Effect Unit
main =
  let
    tellAppRouteChanged _ (Just prev) route | prev == route = pure unit
    tellAppRouteChanged io _ route = void <<< Aff.launchAff $
      App.Query.sendNavigate route io
  in
    HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI (H.hoist App.runM component) unit body
      H.liftEffect <<< void <<< Routing.Hash.matchesWith App.Route.parse $
        tellAppRouteChanged io

component :: ∀ i o. H.Component App.Query.Query i o App.M
component =
  H.mkComponent
    { initialState: const App.State.init
    , render: App.render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just App.Action.Init
        }
    }

timer :: ∀ m a. MonadAff m => Milliseconds -> a -> m (HS.Emitter a)
timer ms val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay ms
    H.liftEffect $ HS.notify listener val
  pure emitter

handleQuery
  :: ∀ a s o
   . App.Query.Query a
  -> H.HalogenM App.State.State App.Action.Action s o App.M (Maybe a)
handleQuery = case _ of
  App.Query.Navigate route _ -> do
    App.put route
    s <- H.get
    H.liftEffect $ Console.log $ show s
    pure Nothing

handleAction
  :: ∀ s o
   . App.Action.Action
  -> H.HalogenM App.State.State App.Action.Action s o App.M Unit
handleAction =
  case _ of
    App.Action.Init -> do
      navigate App.Route.init

      _ <- H.subscribe =<< timer (Milliseconds 100.0) App.Action.Tick

      decl <- H.liftAff $ Concept.fetchDecl
      either (H.liftEffect <<< Console.error) (const <<< pure $ unit) decl

      App.put $ lmap (const "An error occurred fetching concepts.") decl
    App.Action.NavbarSectionPicked n -> navigate (App.Route.ofNavbarSection n)
    App.Action.Tick -> do
      kwapGradientState <- App.State.kwapGradient <$> H.get
      App.put $ App.Css.tick kwapGradientState
    App.Action.Nop -> mempty
