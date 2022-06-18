module Main where

import Prelude
import Title (title)
import Anim (Fade(..), fadeClass)
import Utils (test, maybeArray, snocMaybe, classes)
import Card (CardSize(..))
import Color (bgClass)
import Sections (Section(..), getCards, getColor, allSections)
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
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = { selectedSection :: Section
             , newSection      :: Maybe Section
             , fade            :: Maybe Fade
             }

type Slots = ( title :: forall query. H.Slot query Void Int )

data Action = SectionPicked Section
            | Nop

maybeAction (Just a) = a
maybeAction (Nothing) = Nop

_title = Proxy :: Proxy "title"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
    initialState _ = { selectedSection: Present, newSection: Nothing, fade: Nothing }

    titleSlot title selectedSection = HH.slot _title
                                              0
                                              title
                                              selectedSection
                                              absurd

    render :: State -> H.ComponentHTML Action Slots m
    render { selectedSection, newSection, fade } =
      HH.div
        [ classes [ "app-root" ] ]
        $ (titleSlot title selectedSection)
        : [ appNavbar case newSection of
              Just sec -> sec
              Nothing  -> selectedSection
          ]
        <> appContent selectedSection fade

appNavbar :: forall w. Section -> HH.HTML w Action
appNavbar selectedSection =
  HH.div
    [ classes [ "app-navbar"
              , "flex"
              , "center-children-sec-axis"
              , "stretch-children-main-axis"
              ]
    ]
    (renderNavBtn selectedSection <$> allSections)
  where
    renderNavBtn selected section =
      HH.button
        [ HP.classes [ (getColor section # bgClass)
                     , navBtnClass selected section
                     ]
        , HE.onClick \_ -> maybeAction $ test (section /= selected) (SectionPicked section)
        ]
        [ HH.span_ [ HH.text $ show section ] ]

    navBtnClass section selected = HC.ClassName
      $ case compare selected section of
        EQ -> "selected"
        LT -> "before-selected"
        GT -> "after-selected"

    title = HH.h1
      [ classes [ "huge" ] ]
      [ HH.text "Orion Kindel" ]

appContent :: forall w. Section -> Maybe Fade -> Array (HH.HTML w Action)
appContent selectedSection fade = renderCard <$> getCards selectedSection
  where
    renderCard card =
      HH.div
        [ HP.classes
            $ (HC.ClassName <$> [ "card", "flex", "inline", "vert" ])
            `snocMaybe` classCardSize card
            `snocMaybe` (fadeClass <$> fade)
            `snoc`      (getColor selectedSection # bgClass)
        ]
        [ HH.h1_ [ HH.text card.title ]
        , HH.div [ classes [ "card-content" ] ] (renderCardItem <$> card.items)
        ]

    renderCardItem {title, contents} = HH.div [ classes [ "card-item" ] ]
                                              [ h2Text title, contents ]

    h2Text = HH.h2_ <<< pure <<< HH.text

    classCardSize card = case card.size of
      Large -> Just $ HC.ClassName "lg"
      _     -> Nothing

handleAction :: forall o m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Nop -> mempty
  SectionPicked s -> do
    H.modify_ _ { fade = Just Out
                , newSection = Just s
                }
    _ <- waitMs 250.0
    H.modify_ _ { fade = Just In
                , selectedSection = s
                , newSection = Nothing
                }
    _ <- waitMs 250.0
    H.modify_ _ { fade = Nothing }

  where
    waitMs :: forall state action slots output m. MonadAff m => Number -> H.HalogenM state action slots output m Unit
    waitMs delay =
      H.liftAff $ Aff.delay $ Aff.Milliseconds delay
