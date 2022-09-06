module Toad.Atom.Accordion
  ( Item(..)
  , Title(..)
  , render
  , HeaderItem(..)
  , Config(..)
  ) where

import Toad.Prelude hiding (top)

import CSS.Common (center)
import Data.Active (Active(..))
import Data.Array.Mve as Mve
import Data.Color.OkLab (Lab(..), Lightness(..))
import Data.Expanded (Expanded(..))
import Data.Maybe (fromMaybe, isJust)
import Data.String (joinWith)
import Halogen.HTML.Events (onClick)
import Toad.Atom.Button as Button
import Toad.Atom.Icon as Icon
import Toad.Css
  ( CSS
  , absolute
  , alignItems
  , backgroundColor
  , borderRadius
  , color
  , colorBg
  , colorFg
  , colorPrimary3
  , colorPrimary3_5
  , colorPrimary4
  , colorPrimary5
  , display
  , flex
  , flexGrow
  , flexStart
  , green
  , grey
  , height
  , justifyContent
  , kwapEasing
  , left
  , marginBottom
  , marginLeft
  , marginRight
  , ms
  , oklab
  , padding
  , paddingLeft
  , pct
  , position
  , relative
  , rem
  , spaceBetween
  , style
  , sym
  , top
  , transition
  , width
  )
import Toad.Html as Html

newtype Title = Title String

derive instance newtypeTitle :: Newtype Title _
derive instance genericTitle :: Generic Title _
derive newtype instance eqTitle :: Eq Title
derive newtype instance ordTitle :: Ord Title
instance showTitle :: Show Title where
  show = genericShow

data Item a = Item Title a

instance showItem :: Show a => Show (Item a) where
  show (Item t a) = "Item (" <> show t <> ") (" <> show a <> ")"

data HeaderItem a = HeaderItem Title | HeaderItemWithValue Title a

instance showHeaderItem :: Show a => Show (HeaderItem a) where
  show (HeaderItem t) =
    "HeaderItem ("
      <> show t
      <> ")"
  show (HeaderItemWithValue t a) =
    "HeaderItemWithValue ("
      <> show t
      <> ") ("
      <> show a
      <> ")"

itemData :: ∀ a. Item a -> a
itemData (Item _ a) = a

headerItemData :: ∀ a. HeaderItem a -> Maybe a
headerItemData (HeaderItem _) = Nothing
headerItemData (HeaderItemWithValue _ a) = Just a

headerText :: ∀ a. HeaderItem a -> String
headerText (HeaderItem (Title t)) = t
headerText (HeaderItemWithValue (Title t) _) = t

renderRow
  :: ∀ a w i
   . i
  -> i
  -> (a -> i)
  -> (Lightness -> Lab)
  -> Expanded
  -> Boolean
  -> String
  -> Maybe a
  -> Active
  -> Html.HTML w i
renderRow
  actionNoop
  actionExpand
  actionPickValue
  colorFamily
  expanded
  isHeader
  text
  value
  active =
  let
    squareStyle = do
      height $ rem 2.0
      width $ rem 2.0
      marginRight $ rem 0.25
  in
    Html.div
      [ style do
          display flex
          alignItems center
          marginBottom $ rem 0.25
      ]
      [ Button.render active
          ( Just do
              display flex
              alignItems center
              flexGrow 1.0
              height $ rem 2.0
              paddingLeft $ rem 0.5
          )
          (maybe (if isHeader then actionExpand else actionNoop) actionPickValue value)
          [ Html.h4
              [ style do
                  color ∘ oklab ∘ colorBg $ grey
              ]
              [ Html.text text ]
          ]
      , if not isHeader then
          Html.div
            [ style do
                squareStyle
                position relative
            ]
            []
        else
          Button.render Active
            ( Just do
                squareStyle
                sym padding $ rem 0.25
            )
            actionExpand
            [ Icon.render case expanded of
                Expanded -> Icon.ChevronUp
                Collapsed -> Icon.ChevronDown
            ]
      ]

itemsWithActive
  :: ∀ a
   . Eq a
  => Array (Item a)
  -> a
  -> Maybe (Mve.ArrayMve (Item a))
itemsWithActive is a = Mve.fromArray (eq a ∘ itemData) is

type Config a i =
  { expanded :: Expanded
  , header :: HeaderItem a
  , items :: Array (Item a)
  , active :: Maybe a
  , actionNoop :: i
  , actionToggleExpanded :: i
  , actionClickItem :: a -> i
  }

-- | An accordion where all items correspond to a value of some type `a`,
-- | and where the header is itself an item which may have a value.
-- |
-- | For example, a navigation accordion may have `Route` as the type `a`,
-- | in which case the active item could be the one matching
-- | the browser's current route.
-- |
-- | The active item will be highlighted based on whether it equals an
-- | optionally provided active value.
-- |
-- | `actionToggleExpanded` will still be raised if it contains an active item.
-- |
-- | `actionClickItem` will still be raised if the active item was clicked.
render
  :: ∀ a w i
   . Eq a
  => Maybe CSS
  -> Config a i
  -> Html.HTML w i
render
  x
  { expanded
  , header
  , items
  , active
  , actionNoop
  , actionToggleExpanded
  , actionClickItem
  } =
  let
    rowBase = renderRow
      actionNoop
      actionToggleExpanded
      actionClickItem
      green
      expanded

    headerRow = rowBase
      true
      (headerText header)
      (headerItemData header)
      headerIsActive

    itemRow active' (Item (Title t) a) = rowBase false t (Just a) active'

    itemsWithActive' = itemsWithActive items =<< active
    renderItemsWithActive before activeOne after =
      (itemRow Inactive <$> before)
        <> [ itemRow Active activeOne ]
        <> (itemRow Inactive <$> after)

    headerIsActive
      | headerItemData header == active = Active
      | expanded == Collapsed && isJust itemsWithActive' = Active
      | otherwise = Inactive
  in
    Html.div
      [ style do
          display flex
          justifyContent flexStart
          fromMaybe (pure unit) x
      ]
      [ Html.div
          [ style ∘ flexGrow $ 1.0 ]
          $
            [ headerRow
            ]
          <>
            case expanded of
              Collapsed -> []
              Expanded ->
                [ Html.div
                    [ style do
                        width $ pct 100.0
                    ]
                    $ maybe
                        (itemRow Inactive <$> items)
                        (Mve.foldParts renderItemsWithActive)
                        itemsWithActive'
                ]
      ]
