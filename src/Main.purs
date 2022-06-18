module Main where

import Prelude
import Kwap.Action
import Anim (Fade(..), fadeClass)
import Utils (test, appendFoldable, classes)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console (error)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Array (snoc, (:))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = {}

type Slots
  = ( title :: ∀ query. H.Slot query Void Int )

maybeAction (Just a) = a

maybeAction (Nothing) = Nop

_title = Proxy :: Proxy "title"

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = {}

  render :: State -> H.ComponentHTML Action Slots m
  render {} =
    HH.div
      [ classes [ "app-root" ] ]
      []

handleAction :: ∀ o m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Nop -> mempty
