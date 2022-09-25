module Toad.Page.Concepts (concepts, Input(..), Output(..)) where

import Toad.Prelude

import Data.String (joinWith)
import Effect.Class (class MonadEffect)
import Halogen as H
import Toad.Concept (Ident, Manifest)
import Toad.Css as Css
import Toad.Html as Html
import Toad.Markdown as Md
import Toad.Markdown.Html as Md.Html
import Toad.Page.Concepts.All as Self.All
import Toad.Page.Concepts.One as Self.One
import Toad.Route as Route
import Unsafe.Coerce (unsafeCoerce)

newtype Input = Input
  { hash :: Array Int
  , titleStyle :: Css.CSS
  , bodyStyle :: Css.CSS
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
      , "      , titleStyle: <Css.CSS>"
      , "      , bodyStyle: <Css.CSS>"
      , "      }"
      ]

route :: Input -> Route.OneOrAll Ident
route (Input { route: r }) = r

titleStyle :: Input -> Css.CSS
titleStyle (Input { titleStyle: s }) = s

bodyStyle :: Input -> Css.CSS
bodyStyle (Input { bodyStyle: s }) = s

manifest :: Input -> Maybe Manifest
manifest (Input { manifest: m }) = m

lookupDocument :: Input -> Ident -> Maybe Md.Document
lookupDocument (Input { lookupDocument: l }) = l

data Output = FetchConcept Ident | TitleChanged (Array Html.PlainHTML)

data Action = Init | InputChanged Input

title :: Input -> Maybe (Array Html.PlainHTML)
title i@(Input { route: Route.One id }) = join ∘ map Md.Html.renderHeaderSpan
  ∘ lookupDocument i
  $ id
title _ = Nothing

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
  -> Html.HTML w Action
render i@(Input { route: Route.One id, bodyStyle }) = Self.One.render bodyStyle
  (lookupDocument i id)
render i = Html.text "TODO: may be unnecessary"

handleAction
  :: forall m
   . MonadEffect m
  => Action
  -> H.HalogenM Input Action () Output m Unit
handleAction =
  let
    maybeFetchMissingConcept =
      maybe mempty H.raise
        ∘ map FetchConcept
        ∘ Route.maybeOne
        ∘ route
    maybeTitleChanged =
      do
        maybe mempty H.raise
        ∘ map TitleChanged
        ∘ title
  in
    case _ of
      Init -> do
        input <- H.get
        maybeFetchMissingConcept input
        maybeTitleChanged input
      InputChanged new -> do
        old <- H.get
        when (old /= new) do
          H.put new
          maybeTitleChanged new
          maybeFetchMissingConcept new
