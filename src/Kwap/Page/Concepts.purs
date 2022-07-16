module Kwap.Page.Concepts (concepts, Input(..), Output(..)) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Kwap.Concept (Ident, Manifest)
import Kwap.Css as Css
import Kwap.Html as HH
import Kwap.Page.Concepts.All as Self.All
import Kwap.Page.Concepts.One as Self.One
import Kwap.Route as Route

newtype Input = Input
  { style :: Css.CSS
  , route :: Route.OneOrAll Ident
  , manifest :: Maybe Manifest
  }

instance eqInput :: Eq Input where
  eq (Input { route: ra, manifest: ma }) (Input { route: rb, manifest: mb }) =
    ra == rb && ma == mb

instance showInput :: Show Input where
  show (Input { route: r, manifest: m }) =
    joinWith "\n"
      [ "Input { route: "
      , show r
      , "      , manifest: "
      , show m
      , "      , style: <Css.CSS>"
      , "      }"
      ]

route :: Input -> Route.OneOrAll Ident
route (Input { route: r }) = r

style :: Input -> Css.CSS
style (Input { style: s }) = s

manifest :: Input -> Maybe Manifest
manifest (Input { manifest: m }) = m

data Output = FetchConcept Ident

data Action = Init | InputChanged Input

concepts :: forall q m. MonadEffect m => H.Component q Input Output m
concepts = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< InputChanged
      }
  }

render
  :: ∀ w
   . Input
  -> HH.HTML w Action
render (Input { style: x, route: Route.One _ }) = Self.One.render x Nothing
render (Input { style: x, route: Route.All, manifest: m }) = Self.All.render x m

handleAction
  :: forall m
   . MonadEffect m
  => Action
  -> H.HalogenM Input Action () Output m Unit
handleAction =
  let
    fetchIfNeeded =
      maybe mempty (H.raise <<< FetchConcept)
        <<< Route.maybeOne
        <<< route
  in
    case _ of
      Init -> fetchIfNeeded =<< H.get
      InputChanged new -> do
        old <- H.get
        when (old /= new) do
          H.put new
          fetchIfNeeded new
