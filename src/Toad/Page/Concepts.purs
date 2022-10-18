module Toad.Page.Concepts (concepts, Input(..), Output(..)) where

import Toad.Prelude

import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as Map
import Data.String (joinWith)
import Effect.Class (class MonadEffect)
import Halogen as H
import Toad.Atom.AppTitle (AppTitle(..))
import Toad.Concept (Decl(..), Ident, Manifest, Path(..), decls, path)
import Toad.Css as Css
import Toad.Html as Html
import Toad.Markdown as Md
import Toad.Markdown.Html as Md.Html
import Toad.Route as Route

newtype Input = Input
  { hash :: Array Int
  , titleStyle :: Css.CSS
  , bodyStyle :: Css.CSS
  , route :: Route.OneOrAll Ident
  , manifest :: Maybe Manifest
  , lookupDocument :: Ident -> Maybe Md.Document
  }

newtype State = State
  { input :: Input
  , renderedMarkdown :: Map Ident Html.PlainHTML
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

data Output
  = FetchConcept Ident
  | TitleChanged AppTitle

data Action = Init | InputChanged Input

route :: State -> Route.OneOrAll Ident
route (State { input: Input { route: r } }) = r

titleStyle :: State -> Css.CSS
titleStyle (State { input: Input { titleStyle: s } }) = s

bodyStyle :: State -> Css.CSS
bodyStyle (State { input: Input { bodyStyle: s } }) = s

manifest :: State -> Maybe Manifest
manifest (State { input: Input { manifest: m } }) = m

lookupPath :: State -> Ident -> Maybe Path
lookupPath s id =
  let
    byIdent (Decl { ident }) = ident == id
  in
    pure ∘ path
      =<< find byIdent
      ∘ decls
      =<< manifest s

lookupDocument :: State -> Ident -> Maybe Md.Document
lookupDocument (State { input: Input { lookupDocument: l } }) = l

lookupRendered :: State -> Ident -> Maybe Html.PlainHTML
lookupRendered (State { renderedMarkdown }) = flip Map.lookup renderedMarkdown

title :: State -> Maybe AppTitle
title s@(State { input: Input { route: Route.One id } }) = do
  path <- lookupPath s id
  doc <- lookupDocument s id
  Md.Html.renderAppTitle path doc
title _ = Nothing

concepts :: forall q m. MonadEffect m => H.Component q Input Output m
concepts = H.mkComponent
  { initialState: \i -> State { input: i, renderedMarkdown: Map.empty }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< InputChanged
      }
  }

render
  :: ∀ w
   . State
  -> Html.HTML w Action
render s@(State { input: Input { route: Route.One ident } }) =
  Html.fromPlainHTML
    ∘ maybe (Html.div [] []) id
    $ lookupRendered s ident
render _ = Html.text "TODO: may be unnecessary"

handleAction
  :: forall m
   . MonadEffect m
  => Action
  -> H.HalogenM State Action () Output m Unit
handleAction =
  let
    maybeFetchMissingConcept =
      maybe mempty H.raise
        ∘ map FetchConcept
        ∘ Route.maybeOne
        ∘ route

    maybeTitleChanged =
      maybe mempty H.raise
        ∘ map TitleChanged
        ∘ title

    renderMarkdown css ident doc = do
      html <- H.liftEffect $ Md.Html.renderBody css doc
      State { input, renderedMarkdown } <- H.get
      H.put $ State
        { input, renderedMarkdown: Map.insert ident html renderedMarkdown }
      pure unit

    maybeRenderMarkdown s =
      maybe
        (pure unit)
        id
        do
          ident <- Route.maybeOne $ route s
          document <- lookupDocument s ident
          pure $ renderMarkdown (bodyStyle s) ident document

  in
    case _ of
      Init -> do
        input <- H.get
        maybeFetchMissingConcept input
        maybeTitleChanged input

      InputChanged newInput -> do
        State { input: oldInput, renderedMarkdown } <- H.get

        when (oldInput /= newInput) do
          let new = State { input: newInput, renderedMarkdown }
          H.put new
          maybeTitleChanged new
          maybeFetchMissingConcept new
          maybeRenderMarkdown new
