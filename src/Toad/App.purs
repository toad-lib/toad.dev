module Toad.App (M, Slots, runM, put, render, handleError) where

import Toad.Prelude

import CSS.Common as Css.Common
import CSS.TextAlign (textAlign, rightTextAlign)
import CSS.Overflow (overflow, overflowAuto)
import Data.Color.OkLab (Lightness(..))
import Data.Hashable (hash)
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML.Properties as Prop
import Routing.Hash (setHash)
import Toad.Action (Action(..))
import Toad.App.Navbar as Navbar
import Toad.Atom.Accordion as Accordion
import Toad.Atom.AppTitle as AppTitle
import Toad.Atom.Logo as Logo
import Toad.Concept as Concept
import Toad.Css as Css
import Toad.Css.Grid as Grid
import Toad.Error (Error)
import Toad.Html as Html
import Toad.Navigate (class Navigate)
import Toad.Page.Concepts as Page.Concepts
import Toad.Route as Route
import Toad.State (State)
import Toad.State as State
import Toad.Style as Style
import Toad.Style.Global as Style.Global
import Type.Proxy (Proxy(..))

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
  navigate = liftEffect ∘ setHash ∘ Route.print

--| Given something that is a part of Toad.App.State,
--| replace the existing part with the new part.
--|
--| The rest of the state is unchanged.
put
  :: ∀ a s o m sp
   . State.LiftState sp
  => Monad m
  => sp
  -> H.HalogenM State a s o m Unit
put = (flip bind $ H.put)
  ∘ H.modify
  ∘ append
  ∘ State.liftState

handleError
  :: ∀ a s o m
   . Monad m
  => MonadEffect m
  => Either Error (HalogenM State a s o m Unit)
  -> HalogenM State a s o m Unit
handleError (Right a) = a
handleError (Left (Tuple i u)) = do
  liftEffect ∘ Console.error $ i
  put u
  pure unit

type Slots = (concepts :: forall q. H.Slot q Page.Concepts.Output Int)

_concepts :: Proxy "concepts"
_concepts = Proxy :: Proxy "concepts"

conceptsPageSlot :: Int
conceptsPageSlot = 0

render
  :: ∀ m
   . MonadEffect m
  => State
  -> Html.HTML (H.ComponentSlot Slots m Action) Action
render state =
  Html.div_
    [ Style.Global.stylesheet
    , Html.div
        [ Css.style Style.appWrap
        ]
        [ Html.div
            [ Css.style do
                Css.width ∘ Css.pct $ 100.0
                Css.height ∘ Css.pct $ 100.0
                Css.display Css.flex
                Css.flexDirection Css.column
                overflow overflowAuto
                Style.navbarWrap
            ]
            [ Accordion.render
                Nothing
                { expanded: State.navAccordionsBookExpanded
                    ∘ State.navAccordions
                    $ state
                , header: Accordion.HeaderItem (Accordion.Title "Book")
                , active: Just unit
                , items: []
                , actionToggleExpanded: NavAccordionExpandBook
                , actionClickItem: const Nop
                , actionNoop: Nop
                }
            , Accordion.render
                Nothing
                { expanded: State.navAccordionsConceptsExpanded
                    ∘ State.navAccordions
                    $ state
                , header: Accordion.HeaderItem (Accordion.Title "Concepts")
                , active: Just $ State.route state
                , items:
                    map
                      ( \d -> Accordion.Item
                          ( Accordion.Title
                              ∘ Concept.titleString
                              ∘ Concept.title
                              $ d
                          )
                          ( Route.Concepts
                              ∘ Route.One
                              ∘ Concept.ident
                              $ d
                          )
                      )
                      ∘ maybe [] id
                      ∘ map Concept.decls
                      ∘ State.conceptManifest
                      $ state
                , actionToggleExpanded: NavAccordionExpandConcepts
                , actionClickItem: Navigate
                , actionNoop: Nop
                }
            ]
        , Html.div
            [ Css.style do
                Grid.inAppLogo
                Css.backgroundColor ∘ Css.oklab ∘ Css.green $ Lightness 0.95
                Css.display Css.flex
                Css.alignItems Css.Common.center
                Css.justifyContent Css.spaceBetween
                Css.sym Css.padding $ Css.rem 2.0
            ]
            [ Logo.render do
                  Css.marginRight $ Css.rem 1.0
            , Html.div
                [Css.style do
                    Css.display Css.flex
                    Css.flexDirection Css.column
                    textAlign rightTextAlign
                ] [Html.h1
                [ Css.style do
                    Html.headingStyle
                       $ Html.h1Font
                      <> Css.fontSize Css.FontSizeHuge
                      <> Css.fontFamily Css.WorkSansSemibold
                ]
                [ Html.text "toad" ]
                , Html.h4
                  [ Css.style do
                      Html.headingStyle Html.h4Font
                      Css.color∘Css.oklab∘Css.grey$Lightness 0.40
                  ]
                  [ Html.text "CoAP - HTTP without the bloat" ]
                ]
            ]
        , maybe
            (Html.div [] [])
            (AppTitle.render Grid.inAppContentTitle)
            ∘ _.appTitle
            ∘ State.record
            $ state
        , case State.route state of
            Route.Home -> Html.div_ []
            Route.Concepts oa ->
              Html.slot
                _concepts
                conceptsPageSlot
                Page.Concepts.concepts
                ( Page.Concepts.Input
                    { hash:
                        [ State.routeHash
                        , State.conceptManifestHash
                        , State.conceptsHash
                        , hash ∘ _.appTitle ∘ State.record
                        ]
                          <*> [ state ]
                    , route: oa
                    , manifest: State.conceptManifest state
                    , titleStyle: Grid.inAppContentTitle
                    , bodyStyle:
                        if Nothing == (_.appTitle ∘ State.record $ state) then
                          Grid.inAppContentAndTitle
                        else Grid.inAppContent
                    , lookupDocument: (flip Map.lookup) $ State.concepts state
                    }
                )
                ConceptsPageOutput
            Route.Book -> Html.div_ []
        ]
    ]
