module Kwap.App (M, runM, put, render, handleError) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (T3, (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (HalogenM(..))
import Halogen as H
import Kwap.Action (Action(..))
import Kwap.Css as Css
import Kwap.Css.Grid as Grid
import Kwap.Error (Error)
import Kwap.Html as HH
import Kwap.Layout (AppLayout(..))
import Kwap.Navbar as Navbar
import Kwap.Navbar.Toast as Toast
import Kwap.Navigate (class Navigate)
import Kwap.Page.Concepts as Page.Concepts
import Kwap.Route as Route
import Kwap.State as State
import Kwap.Style as Style
import Kwap.Style.Global as Style.Global
import Routing.Hash (setHash)
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
  :: forall a s o m
   . Monad m
  => MonadEffect m
  => Either Error (HalogenM State.State a s o m Unit)
  -> HalogenM State.State a s o m Unit
handleError (Right a) = a
handleError (Left (Tuple i u)) = do
  liftEffect <<< Console.error $ i
  put u
  pure unit

type Slots = (concepts :: forall q. H.Slot q Page.Concepts.Output Int)

_concepts = Proxy :: Proxy "concepts"

toast :: State.State -> Maybe (T3 Toast.Status String Action)
toast = State.error >>> map ((Toast.StatusError /\ _) <<< (_ /\ DismissError))

render
  :: ∀ m
   . MonadEffect m
  => State.State
  -> HH.HTML (H.ComponentSlot Slots m Action) Action
render state =
  HH.div_
    [ Style.Global.stylesheet
    , HH.div
        [ Css.style $ Style.appBackground $ State.kwapGradient state
        ]
        []
    , HH.div
        [ Css.style Style.appWrap
        ]
        [ HH.div
            [ Css.style Style.navbarWrap
            ]
            [ Navbar.render (toast state) NavbarSectionPicked AppLayoutDesktop
                (State.navbarSection state)
            ]
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
