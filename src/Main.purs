module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Kwap.App as App
import Kwap.App.Action (Action(..))
import Kwap.App.Css (tick)
import Kwap.App.State as App.State

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: mempty
    , render: App.render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Init }
    }

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
  -> H.HalogenM App.State.State Action s o m Unit
handleAction =
  let
    modifyPartial
      :: forall s' o' m' sp
       . App.State.LiftState sp
      => sp
      -> H.HalogenM App.State.State Action s' o' m' App.State.State
    modifyPartial = H.modify <<< append <<< App.State.liftState
  in
    case _ of
      Init -> do
        _ <- H.subscribe =<< timer Tick
        mempty
      NavbarSectionPicked n -> H.put =<< modifyPartial n
      Tick -> do
        kwapGradientState <- App.State.kwapGradient <$> H.get
        H.put =<< (modifyPartial $ tick kwapGradientState)
      Nop -> mempty
