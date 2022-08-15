module Toad.App (M, runM, put, render, handleError) where

import Toad.Prelude

import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (HalogenM)
import Halogen as H
import Routing.Hash (setHash)
import Toad.Action (Action(..))
import Toad.App.Navbar as Navbar
import Toad.Css as Css
import Toad.Css.Grid as Grid
import Toad.Error (Error)
import Toad.Html as HH
import Toad.Navigate (class Navigate)
import Toad.Page.Concepts as Page.Concepts
import Toad.Route as Route
import Toad.State as State
import Toad.Style as Style
import Toad.Style.Global as Style.Global
import Type.Proxy (Proxy(..))

newtype M a = M (Aff a)

runM :: ∀ a. M a -> Aff a
runM (M a) = a

derive newtype instance functorM :: Functor M
derive newtype instance applyM :: Apply M
derive newtype instance applicativeM :: Applicative M
derive newtype instance bindM :: Bind M
derive newtype instance monadM :: Monad M
derive newtype instance monadEffectM :: MonadEffect M
derive newtype instance monadAffM :: MonadAff M

instance navigateM :: Navigate M where
  navigate = liftEffect <<< setHash <<< Route.print

put
  :: ∀ a s o m sp
   . State.LiftState sp
  => Monad m
  => sp
  -> H.HalogenM State.State a s o m Unit
put = (flip bind $ H.put) <<< H.modify <<< append <<< State.liftState

handleError
  :: ∀ a s o m
   . Monad m
  => MonadEffect m
  => Either Error (HalogenM State.State a s o m Unit)
  -> HalogenM State.State a s o m Unit
handleError (Right a) = a
handleError (Left (Tuple i u)) = do
  liftEffect <<< Console.error $ i
  put u
  pure unit

type Slots = (concepts :: ∀ q. H.Slot q Page.Concepts.Output Int)

_concepts = Proxy :: Proxy "concepts"

render
  :: ∀ m
   . MonadEffect m
  => State.State
  -> HH.HTML (H.ComponentSlot Slots m Action) Action
render state =
  HH.div_
    [ Style.Global.stylesheet
    , HH.div
        [ Css.style Style.appWrap
        ]
        [ HH.div
            [ Css.style Style.navbarWrap ]
            [ Navbar.render Navbar.dummyExpanded Navbar.dummyItems ]
        , case State.route state of
            Route.Home -> HH.div_ []
            Route.Concepts oa ->
              HH.slot
                _concepts
                0
                Page.Concepts.concepts
                ( Page.Concepts.Input
                    { hash:
                        [ State.routeHash
                        , State.conceptManifestHash
                        , State.conceptsHash
                        ] <*> [ state ]
                    , route: oa
                    , manifest: State.conceptManifest state
                    , style: Grid.inAppContent
                    , lookupDocument: (flip Map.lookup) $ State.concepts state
                    }
                )
                ConceptsPageOutput
            Route.Book -> HH.div_ []
        ]
    ]
