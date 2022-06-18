module Title where

import Prelude
import Utils (classes, snocMaybe, test)
import Anim (Fade(..), fadeClass)
import Color (bgClass)
import Sections (Section, getColor)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Data.Traversable (traverse)
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Data.Array (replicate, snoc, tail, elem, length, (!!))
import Web.HTML.HTMLElement as El
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

newtype PrevText = PrevText (Array String)

derive instance newtypePrevText :: Newtype PrevText _

updatePrev :: String -> PrevText -> PrevText
updatePrev new (PrevText prev) | length prev >= 3 = PrevText $ tailOrEmpty prev `snoc` new
                               | otherwise        = PrevText $ prev `snoc` new

-- | Get the tail of an array, and return empty array
-- | if input array was empty
tailOrEmpty :: forall a. Array a -> Array a
tailOrEmpty arr = case tail arr of
  Just arrTail -> arrTail
  Nothing -> []

type State = { text          :: String
             , nextText      :: Maybe String
             , nextTextWidth :: Maybe Number
             , huge          :: Boolean
             , nextHuge      :: Boolean
             , prev          :: PrevText
             , section       :: Section
             , fade          :: Maybe Fade
             }

data Action = ChooseNewText
            | Initialize
            | Finalize
            | SectionChanged Section

oopsText :: String
oopsText = "Oops! My code farted."

containerRef :: H.RefLabel
containerRef = H.RefLabel "container"

title :: forall q o m. MonadAff m => H.Component q Section o m
title =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     , finalize = Just Finalize
                                     , receive = Just <<< SectionChanged
                                     }
    }
  where
    initialState section = { text: defaultText
                           , prev: PrevText []
                           , section
                           , nextText: Nothing
                           , nextTextWidth: Nothing
                           , fade: Nothing
                           , huge: true
                           , nextHuge: true
                           }

    render :: State -> H.ComponentHTML Action () m
    render { text, huge, nextHuge, section, nextText, nextTextWidth, fade } = HH.div
          [ classes [ "app-title"
                    , "flex"
                    , "center-children-sec-axis"
                    , "relative"
                    ]
          , HP.ref containerRef
          ]
          ([ HH.h1 [ HP.classes $ [HC.ClassName "app-title-text"]
                                    `snocMaybe` (test huge $ HC.ClassName "huge")
                                    `snocMaybe` (fade <#> fadeClass)
                   ]
                   [ HH.text text ]
           , HH.div ([ HP.classes $ [ HC.ClassName "ribbon", (getColor >>> bgClass) section ]
                     ]
                       `snocMaybe` (((_ + 32.0) >>> width) <$> nextTextWidth)
                    )
                    []
           ]
           `snocMaybe` (nextText <#> (nextTextH1 nextHuge))
          )

    width :: forall r i. Number -> HP.IProp r i
    width w = HP.attr (HC.AttrName "style") ("width: " <> (show w) <> "px;")

    nextTextH1 :: forall w i. Boolean -> String -> HH.HTML w i
    nextTextH1 huge nextText = HH.h1 [ HP.classes $ (HC.ClassName <$> [ "title-width-hack" ]) `snocMaybe` (test huge $ HC.ClassName "huge") ]
                                     [ HH.text nextText ]

type EvalM o m = H.HalogenM State Action () o m Unit

handleAction :: forall o m. MonadAff m => Action -> EvalM o m
handleAction =
  let
    randomText :: Array String -> Effect String
    randomText prev = do
      ix <- randomInt 0 textOptionsMaxIx
      case textOptions !! ix of
        Just text | elem text prev -> randomText prev
                  | otherwise      -> pure text
        otherwise                  -> pure oopsText

    waitMs :: forall o m. MonadAff m => Number -> EvalM o m
    waitMs num = H.liftAff $ Aff.delay $ Aff.Milliseconds num

    query :: String -> El.HTMLElement -> Effect (Maybe El.HTMLElement)
    query selector = El.toParentNode >>> (querySelector (QuerySelector selector)) >>> (map \m -> m >>= El.fromElement)

    queryWidth :: String -> El.HTMLElement -> Effect (Maybe Number)
    queryWidth selector el = do
      queried <- query selector el
      traverse El.offsetWidth queried
  in
    case _ of
      ChooseNewText -> do
        {prev} <- H.get
        -- generate next text
        newText <- H.liftEffect $ randomText $ unwrap prev
        -- get container ref
        container <- H.getHTMLElementRef containerRef
        -- update state with next text
        H.modify_ _ { nextText = Just newText
                    , prev = updatePrev newText prev
                    }

        -- get width of container
        containerWidth <- H.liftEffect $ traverse El.offsetWidth container
        -- get next text h1 el
        textWidth <- H.liftEffect $ case container of
                                       Just el -> queryWidth "h1.app-title-text" el
                                       Nothing -> pure Nothing

        H.modify_ _ {nextTextWidth = textWidth}
        -- get next text h1 el
        nextTextWidth <- H.liftEffect $ case container of
                                          Just el -> queryWidth "h1.title-width-hack" el
                                          Nothing -> pure Nothing
        -- does it spill out of the container?
        let huge = case [nextTextWidth, containerWidth] of
                     [Just nw, Just cw] -> nw <= (cw - 64.0)
                     otherwise          -> true
        s <- H.get

        -- if so, make next text smaller
        case huge == s.nextHuge of
          false -> H.modify_ _ {nextHuge = huge}
          otherwise -> mempty

        -- recheck width
        nextTextWidth2 <- H.liftEffect $ case container of
                                          Just el -> queryWidth "h1.title-width-hack" el
                                          Nothing -> pure Nothing
        _ <- waitMs 5000.0

        -- fade out
        H.modify_ _ { fade = Just Out
                    , nextTextWidth = nextTextWidth2
                    }
        _ <- waitMs 250.0

        -- fade in
        H.modify_ _ { fade = Just In, huge = huge, text = newText }
        _ <- waitMs 250.0

        -- settled
        H.modify_ _ { fade = Nothing, text = newText }

        -- aw shit, here we go again
        handleAction ChooseNewText
      Initialize -> handleAction ChooseNewText
      Finalize -> mempty
      SectionChanged section -> H.modify_ _ { section = section }

newtype Weight = Weight Int
derive instance newtypeWeight :: Newtype Weight _

data TextOption = TextOption Weight String

defaultText :: String
defaultText = "Orion Kindel"

textOptionsMaxIx :: Int
textOptionsMaxIx = length textOptions - 1

textOptions :: Array String
textOptions = do
  TextOption weight text <- textOptionsWeighted
  replicate (unwrap weight) text

textOptionsWeighted :: Array TextOption
textOptionsWeighted = [ TextOption (Weight 20) defaultText
                      , TextOption (Weight 15) "Always Learning"
                      , TextOption (Weight 15) "User Advocate"
                      , TextOption (Weight 15) "Beginner's Mind"
                      , TextOption (Weight 15) "Product Engineer"
                      , TextOption (Weight 15) "Master of None"
                      , TextOption (Weight 10) "Teacher"
                      , TextOption (Weight 10) "Swiss Army Knife"
                      , TextOption (Weight 10) "Student"
                      , TextOption (Weight 5 ) "Open-Source Lover"
                      , TextOption (Weight 2 ) "Glad you're here"
                      , TextOption (Weight 1 ) "Average Climber"
                      , TextOption (Weight 1 ) "<buzzwords here>"
                      ]
