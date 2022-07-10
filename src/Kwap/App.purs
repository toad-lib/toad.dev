module Kwap.App (M, runM, put, render) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Kwap.Action (Action(..))
import Kwap.Css as Css
import Kwap.Css.Grid as Grid
import Kwap.Html as HH
import Kwap.Layout (AppLayout(..))
import Kwap.Navbar as Navbar
import Kwap.Page.Concepts as Page.Concepts
import Kwap.Route as Route
import Kwap.State as State
import Kwap.Style as Style
import Kwap.Style.Global as Style.Global
import Kwap.Navigate (class Navigate)
import Routing.Hash (setHash)

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
  :: ∀ a s o sp
   . State.LiftState sp
  => sp
  -> H.HalogenM State.State a s o M Unit
put = (flip bind $ H.put) <<< H.modify <<< append <<< State.liftState

render :: ∀ w. State.State -> HH.HTML w Action
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
            [ Navbar.render NavbarSectionPicked AppLayoutDesktop
                (State.navbarSection state)
            ]
        , case State.route state of
            Route.Home -> HH.div_ []
            Route.Concepts oa ->
              Page.Concepts.render oa Grid.inAppContent $
                State.conceptDecl state
            Route.Book -> HH.div_ []
        ]
    ]
