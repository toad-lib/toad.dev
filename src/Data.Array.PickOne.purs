module Data.Array.Mve
  ( ArrayMve
  , toArray
  , fromArray
  , changeMve
  , mve
  , parts
  , foldParts
  , singleton
  ) where

import Prelude

import Data.Array (index, last, slice, splitAt)
import Data.Foldable (class Foldable, findMap, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (T3, (/\))
import Partial.Unsafe (unsafePartial)

-- | A non-empty array with a pointer to exactly one element
-- | allowing for constant time lookups of this element.
-- |
-- | Note that combinator typeclass instances (apply, bind, traverse, semigroup)
-- | are omitted, because it would mean discarding one or more MVEs.
-- | (in MVE <> MVE, which pointer do we keep?)
-- |
-- | These operations should instead be done by dropping into an Array
-- | with `toArray`, then lifting back into an ArrayMve with `fromArray`.
data ArrayMve a = ArrayMve (Array a) Int

instance functorMve :: Functor ArrayMve where
  map f (ArrayMve a i) = unsafeFromIndexArray i <<< map f $ a

instance foldMve :: Foldable ArrayMve where
  foldl f b = foldl f b <<< toArray
  foldr f b = foldr f b <<< toArray
  foldMap f = foldMap f <<< toArray

instance semigroupMve :: Semigroup (ArrayMve a) where
  append a b = unsafeModifyArray (_ <> toArray b) a

unsafeFromIndexArray :: forall a. Int -> Array a -> ArrayMve a
unsafeFromIndexArray = flip ArrayMve

unsafeFromArrayIndex :: forall a. Array a -> Int -> ArrayMve a
unsafeFromArrayIndex = ArrayMve

unsafeModifyArray
  :: forall a b. (Array a -> Array b) -> ArrayMve a -> ArrayMve b
unsafeModifyArray f (ArrayMve a i) = ArrayMve (f a) i

enumerate :: forall a. Array a -> Array (Tuple a Int)
enumerate = foldl
  ( \b a' -> b <>
      [ a' /\ (fromMaybe 0 <<< map (add 1 <<< snd) $ last b) ]
  )
  mempty

fromArray :: forall a. (a -> Boolean) -> Array a -> Maybe (ArrayMve a)
fromArray f a =
  map (unsafeFromArrayIndex a)
    <<< findMap (\(Tuple a' ix) -> if f a' then Just ix else Nothing)
    <<< enumerate
    $ a

toArray :: forall a. ArrayMve a -> Array a
toArray (ArrayMve a _) = a

changeMve :: forall a. (a -> Boolean) -> ArrayMve a -> Maybe (ArrayMve a)
changeMve f = toArray >>> fromArray f

mve :: forall a. ArrayMve a -> a
mve (ArrayMve a i) = unsafePartial fromJust <<< index a $ i

-- | Split the MVE into a 3-tuple of (elements before mve, mve, elements after mve)
parts :: forall a. ArrayMve a -> T3 (Array a) a (Array a)
parts am@(ArrayMve a i) = slice 0 i a /\ mve am /\ (splitAt (i + 1) a).after

-- | Apply the result of `parts` to a ternary function
foldParts :: forall a b. (Array a -> a -> Array a -> b) -> ArrayMve a -> b
foldParts f = parts >>> (\(Tuple a (Tuple b c)) -> f a b c)

singleton :: forall a. a -> ArrayMve a
singleton a = ArrayMve [ a ] 0
