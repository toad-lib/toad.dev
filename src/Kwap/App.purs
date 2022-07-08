module Kwap.App (M, runM, put, render) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Kwap.App.Action (Action(..))
import Kwap.App.Content as App.Content
import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Layout (AppLayout(..))
import Kwap.App.Navbar as App.Navbar
import Kwap.App.Route as App.Route
import Kwap.App.State as App.State
import Kwap.App.Style as App.Style
import Kwap.App.Style.Global as App.Style.Global
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
  navigate = liftEffect <<< setHash <<< App.Route.print

put
  :: ∀ a s o sp
   . App.State.LiftState sp
  => sp
  -> H.HalogenM App.State.State a s o M Unit
put = (flip bind $ H.put) <<< H.modify <<< append <<< App.State.liftState

render :: ∀ w. App.State.State -> HH.HTML w Action
render state =
  HH.div_
    [ App.Style.Global.stylesheet
    , HH.div
        [ Css.style $ App.Style.appBackground $ App.State.kwapGradient state
        ]
        []
    , HH.div
        [ Css.style App.Style.appWrap
        ]
        [ HH.div
            [ Css.style App.Style.navbarWrap
            ]
            [ App.Navbar.render NavbarSectionPicked AppLayoutDesktop
                (App.State.navbarSection state)
            ]
        , HH.div
            [ Css.style App.Style.contentWrap
            ]
            [ App.Content.render
            ]
        ]
    ]
