module Main where

import Kwap.App.Css
import Prelude hiding (top)

import CSS.Common as Css.Common
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Kwap.App.Action (Action(..))
import Kwap.App.Css.Grid as App.Css.Grid
import Kwap.App.Html as HH
import Kwap.App.Layout (AppLayout(..))
import Kwap.App.Style as App.Style
import Kwap.App.Navbar as App.Navbar

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = { kwapGradient :: KwapGradient
             , navbarSection :: App.Navbar.Section
             }

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState _ = { kwapGradient: kwapGradientInit, navbarSection: App.Navbar.Home }

  render :: State -> H.ComponentHTML Action _ m
  render { kwapGradient: grabent, navbarSection } =
    let
      appBackground =
        HH.div
          [ style do
              width $ pct 100.0
              height $ pct 100.0
              position fixed
              top $ px 0.0
              left $ px 0.0
              zIndex $ -1000
              kwapGradient grabent
          ]
          []
      appContainer =
        HH.div
          [ style do
              position fixed
              width $ pct 100.0
              height $ pct 100.0
              top $ px 0.0
              left $ px 0.0
              App.Css.Grid.appGrid AppLayoutDesktop
          ]
      appContent =
        HH.div
          [ style do
              App.Css.Grid.inAppContent
              backgroundColor $ Yellow Lightest
          ]
      appNavbar =
        HH.div
          [ style do
              border solid (rem 2.0) (cssColor $ Yellow Lightest)
              App.Css.Grid.inAppNavbar
          ]
    in
      HH.div_
        [ App.Style.global
        , appBackground
        , appContainer
            [ appNavbar [ App.Navbar.render NavbarSectionPicked AppLayoutDesktop navbarSection ]
            ]
        ]

timer :: ∀ m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 200.0
    H.liftEffect $ HS.notify listener val
  pure emitter

handleAction
  :: ∀ s o m
   . MonadEffect m
  => MonadAff m
  => Action
  -> H.HalogenM State Action s o m Unit
handleAction = case _ of
  Init -> do
    _ <- H.subscribe =<< timer Tick
    mempty
  NavbarSectionPicked n -> H.put =<< H.modify _ { navbarSection = n }
  Tick -> do
    { kwapGradient: kwapGradient' } <- H.get
    H.put =<< H.modify _ { kwapGradient = tick kwapGradient' }
  Nop -> mempty
