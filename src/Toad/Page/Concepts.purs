module Toad.Page.Concepts (concepts, Input(..), Output(..)) where

import Toad.Prelude

import Data.String (joinWith)
import Effect.Class (class MonadEffect)
import Halogen as H
import Toad.Concept (Ident, Manifest)
import Toad.Css as Css
import Toad.Html as HH
import Toad.Markdown as Md
import Toad.Page.Concepts.All as Self.All
import Toad.Page.Concepts.One as Self.One
import Toad.Route as Route

newtype Input = Input
  { hash :: Array Int
  , style :: Css.CSS
  , route :: Route.OneOrAll Ident
  , manifest :: Maybe Manifest
  , lookupDocument :: Ident -> Maybe Md.Document
  }

instance eqInput :: Eq Input where
  eq (Input { hash: ha }) (Input { hash: hb }) = ha == hb

instance showInput :: Show Input where
  show (Input { hash: h, route: r, manifest: m }) =
    joinWith "\n"
      [ "Input { hash: " <> show h
      , "      , route: " <> show r
      , "      , manifest: " <> show m
      , "      , lookupDocument: <Ident -> Maybe Md.Document>"
      , "      , style: <Css.CSS>"
      , "      }"
      ]

route :: Input -> Route.OneOrAll Ident
route (Input { route: r }) = r

style :: Input -> Css.CSS
style (Input { style: s }) = s

manifest :: Input -> Maybe Manifest
manifest (Input { manifest: m }) = m

lookupDocument :: Input -> Ident -> Maybe Md.Document
lookupDocument (Input { lookupDocument: l }) = l

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
render i@(Input { route: Route.One id }) = Self.One.render (style i)
  (lookupDocument i id)
render i = Self.All.render (style i) (manifest i)

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
