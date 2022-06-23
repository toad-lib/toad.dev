module Main where

import Kwap.Action
import Kwap.App.Css
import Prelude hiding (top)

import Anim (Fade(..), fadeClass)
import CSS.Size as Css.Size
import Control.Monad.Rec.Class (forever)
import Data.Array (head, snoc, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Kwap.App.Html as HH
import Type.Proxy (Proxy(..))
import Utils (appendFoldable, classes, test)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = { kwapGradient :: KwapGradient }

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState _ = { kwapGradient: kwapGradientInit }

  render :: State -> H.ComponentHTML Action _ m
  render { kwapGradient: grabent } =
    let
      backgroundLayer0 =
        HH.div
          [ style do
              width $ vw 100.0
              height $ vh 100.0
              position absolute
              top $ Css.Size.unitless 0.0
              left $ Css.Size.unitless 0.0
              zIndex (-1000)
              kwapGradient grabent
          ]
          []
      backgroundLayer1 =
        HH.div
          [ style do
              position absolute
              width $ pct 100.0 @-@ rem 4.0
              height $ pct 80.0 @-@ rem 4.0
              top $ rem 1.0
              left $ rem 1.0
              zIndex (-999)
              padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)
              borderRadius (rem 2.0) (rem 2.0) (rem 2.0) (rem 2.0)
              backgroundColor $ Yellow Lightest
          ]
    in
      HH.div_
        [ backgroundLayer0
        , backgroundLayer1
            [ HH.h1 `HH.withText` "kwap is the stuff"
            , HH.h2 `HH.withText` "the guy to know, the place to be"
            , HH.h3 `HH.withText` "i love you, you love me, tony hawk's pro skater 3"
            , HH.h5 `HH.withText` "and a knick knack paddywhack give the kick a flip"
            , HH.p `HH.withText`
                "kwap is a universal implementation of CoAP - the fast, safe, and low-latency HTTP alternative."
            ]
        ]

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 100.0
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
  Tick -> do
    { kwapGradient: kwapGradient' } <- H.get
    H.put { kwapGradient: tick kwapGradient' }
  Nop -> mempty
