module Toad.Prelude (module X) where

import Data.Coord.Polar (Degrees(..), Radians(..)) as X
import Data.Either (Either(..)) as X
import Data.Eq.Generic (genericEq) as X
import Data.Foldable (class Foldable, fold, foldl, foldr) as X
import Data.Generic.Rep (class Generic) as X
import Data.Maybe (Maybe(..), maybe) as X
import Data.Number (pi) as X
import Data.Show.Generic (genericShow) as X
import Data.Traversable (class Traversable, sequence, traverse) as X
import Data.Tuple (Tuple(..)) as X
import Data.Tuple.Nested ((/\)) as X
import Effect (Effect) as X
import Prelude
  ( class Applicative
  , class Apply
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class Category
  , class CommutativeRing
  , class Discard
  , class DivisionRing
  , class Eq
  , class EuclideanRing
  , class Field
  , class Functor
  , class HeytingAlgebra
  , class Monad
  , class Monoid
  , class Ord
  , class Ring
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , type (~>)
  , Ordering(..)
  , Unit
  , Void
  , absurd
  , add
  , ap
  , append
  , apply
  , between
  , bind
  , bottom
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , degree
  , discard
  , disj
  , div
  , eq
  , flap
  , flip
  , gcd
  , identity
  , ifM
  , join
  , lcm
  , liftA1
  , liftM1
  , map
  , max
  , mempty
  , min
  , mod
  , mul
  , negate
  , not
  , notEq
  , one
  , otherwise
  , pure
  , recip
  , show
  , sub
  , top
  , unit
  , unless
  , unlessM
  , void
  , when
  , whenM
  , zero
  , (#)
  , ($)
  , ($>)
  , (&&)
  , (*)
  , (*>)
  , (+)
  , (-)
  , (/)
  , (/=)
  , (<)
  , (<#>)
  , (<$)
  , (<$>)
  , (<*)
  , (<*>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<@>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (||)
  ) as X
