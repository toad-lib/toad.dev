module Toad.App.Navbar where

import Toad.Prelude

import CSS.Common as Css.Common
import Data.Array as Array
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype, unwrap)
import Toad.App.Navbar.Internal
  ( Active(..)
  , ChildIs(..)
  , SiblingIs(..)
  , Expanded(..)
  , Visible(..)
  )
import Toad.App.Navbar.Style as Style
import Toad.Css as Css
import Toad.Html as HH

newtype Title = Title String

derive instance newtypeTitle :: Newtype Title _

newtype RouteSegment = RouteSegment String

derive instance newtypeRouteSegment :: Newtype RouteSegment _
derive newtype instance eqRouteSegment :: Eq RouteSegment
derive newtype instance semiRouteSegment :: Semigroup RouteSegment
derive newtype instance monoidRouteSegment :: Monoid RouteSegment
derive newtype instance hashRouteSegment :: Hashable RouteSegment

newtype Route = Route (Array RouteSegment)

derive instance newtypeRoute :: Newtype Route _
derive newtype instance eqRoute :: Eq Route
derive newtype instance semiRoute :: Semigroup Route
derive newtype instance monoidRoute :: Monoid Route
derive newtype instance hashRoute :: Hashable Route

data Item = Node Title RouteSegment (Array Item) | Leaf Title RouteSegment

newtype Depth = Depth Int

derive instance newtypeDepth :: Newtype Depth _
derive newtype instance eqDepth :: Eq Depth
derive newtype instance semiringDepth :: Semiring Depth
derive newtype instance ringDepth :: Ring Depth

newtype ExpandedItems = ExpandedItems (HashSet Route)

derive instance ntExpandedItems :: Newtype ExpandedItems _

data FlatItem = FlatItem Depth Title Route

routeStartsWith :: Route -> Route -> Boolean
routeStartsWith (Route []) (Route _) = false
routeStartsWith (Route a) (Route b)
  | a == b = true
  | otherwise = routeStartsWith (Route <<< fold <<< Array.init $ a) (Route b)

flattenItems :: Route -> Depth -> Array Item -> Array FlatItem
flattenItems _ _ [] = []
flattenItems rs d is = Array.concat <<< map (flattenItem rs d) $ is

flattenItem :: Route -> Depth -> Item -> Array FlatItem
flattenItem (Route rs) d (Leaf t r) = pure $ FlatItem d t (Route $ rs <> pure r)
flattenItem (Route rs) d (Node t r is) =
  append
    (pure $ FlatItem d t (Route $ rs <> pure r))
    (flattenItems (Route $ rs <> pure r) (d + Depth 1) is)

dummyExpanded :: ExpandedItems
dummyExpanded = ExpandedItems
  (HashSet.singleton (Route [ RouteSegment "welcome" ]))

dummyItems :: Array Item
dummyItems =
  [ Node
      (Title "Welcome!")
      (RouteSegment "welcome")
      [ Node (Title "Foo") (RouteSegment "foo")
          [ Leaf (Title "Fracking") (RouteSegment "fracking")
          , Leaf (Title "Shark") (RouteSegment "shark")
          , Leaf (Title "Snails") (RouteSegment "snails")
          ]
      ]
  , Node (Title "Book") (RouteSegment "book") []
  , Node (Title "Concepts") (RouteSegment "concept") []
  ]

renderItemRows
  :: ∀ w i. Route -> ExpandedItems -> Array FlatItem -> Array (HH.HTML w i)
renderItemRows r x is =
  let
    siblingIsActive (FlatItem _ _ (Route r')) = case Array.init <<< unwrap $ r, Array.init r' of
      Just ir, Just ir' | ir == ir' -> SiblingIs Active
      _, _ -> SiblingIs Inactive

    childIsExpanded r' =
      Array.any (_ `routeStartsWith` r')
        <<< HashSet.toArray
        <<< unwrap
        $ x

    childIsActive (FlatItem _ _ r')
      | r `routeStartsWith` r' = ChildIs Active
      | otherwise = ChildIs Inactive

    active (FlatItem _ _ r')
      | r' == r = Active
      | otherwise = Inactive

    vis i@(FlatItem d _ r')
      | d == Depth 0 = Visible
      | active i == Active = Visible
      | childIsExpanded r' = Visible
      | childIsActive i == ChildIs Active = Visible
      | siblingIsActive i == SiblingIs Active = Visible
      | otherwise = Hidden

    renderItem' i = renderItem (siblingIsActive i) (childIsActive i) (active i) (vis i) i
  in
    renderItem' <$> is

renderItem
  :: ∀ w i
   . SiblingIs Active
  -> ChildIs Active
  -> Active
  -> Visible
  -> FlatItem
  -> HH.HTML w i
renderItem sa ca a e (FlatItem d t _) =
  HH.div
    [ Css.style $ Css.display Css.flex ]
    [ HH.div [ Css.style $ Style.itemRibbon sa ca ] []
    , HH.div
        [ Css.style $ Style.itemWrapper d e a ]
        [ HH.div [Css.style Style.itemUnderline] []
        , HH.h4 [ Css.style $ Style.itemText a ] [ HH.text <<< unwrap $ t ]
        ]
    ]

render :: ∀ w i. ExpandedItems -> Array Item -> HH.HTML w i
render x is =
  HH.div
    [ Css.style Style.navbarWrapper ]
    [ HH.div
        [ Css.style Style.navbar ]
        ( renderItemRows
            ( Route
                [ RouteSegment "welcome"
                , RouteSegment "foo"
                , RouteSegment "shark"
                ]
            )
            x
            (flattenItems mempty zero is)
        )
    ]
