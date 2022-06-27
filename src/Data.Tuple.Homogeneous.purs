module Data.Tuple.Homogeneous where

import Prelude

import Data.Array as Array
import Data.Tuple
import Data.Tuple.Nested

class FixedTuple t where
  arrayOf :: ∀ a. t a -> Array a

data FT1 a =  FT1 a
data FT2 a =  FT2 a (FT1 a)
data FT3 a =  FT3 a (FT2 a)
data FT4 a =  FT4 a (FT3 a)
data FT5 a =  FT5 a (FT4 a)
data FT6 a =  FT6 a (FT5 a)
data FT7 a =  FT7 a (FT6 a)
data FT8 a =  FT8 a (FT7 a)
data FT9 a =  FT9 a (FT8 a)
data FT10 a = FT10 a (FT9 a)

ftuple1 :: ∀ a. a -> FT1 a
ftuple1 = FT1

ftuple2 :: ∀ a. a -> a -> FT2 a
ftuple2 a = ftuple1 >>> FT2 a

ftuple3 :: ∀ a. a -> a -> a -> FT3 a
ftuple3 a b = ftuple2 b >>> FT3 a

ftuple4 :: ∀ a. a -> a -> a -> a -> FT4 a
ftuple4 a b c = ftuple3 b c >>> FT4 a

ftuple5 :: ∀ a. a -> a -> a -> a -> a -> FT5 a
ftuple5 a b c d = ftuple4 b c d >>> FT5 a

ftuple6 :: ∀ a. a -> a -> a -> a -> a -> a -> FT6 a
ftuple6 a b c d e = ftuple5 b c d e >>> FT6 a

ftuple7 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> FT7 a
ftuple7 a b c d e f = ftuple6 b c d e f >>> FT7 a

ftuple8 :: ∀ a. a -> a -> a -> a -> a-> a  -> a -> a -> FT8 a
ftuple8 a b c d  e f g= ftuple7 b c d e f g >>> FT8 a

ftuple9 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> FT9 a
ftuple9 a b c d e f g h = ftuple8 b c d e f g h >>> FT9 a

ftuple10 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> FT10 a
ftuple10 a b c d e f g h i = ftuple9 b c d e f g h i >>> FT10 a

instance ft1Fixed :: FixedTuple FT1 where
  arrayOf (FT1 a) = [a]

instance ft2Fixed :: FixedTuple FT2 where
  arrayOf (FT2 a rest) = [a] <> arrayOf rest

instance ft3Fixed :: FixedTuple FT3 where
  arrayOf (FT3 a rest) = [a] <> arrayOf rest

instance ft4Fixed :: FixedTuple FT4 where
  arrayOf (FT4 a rest) = [a] <> arrayOf rest

instance ft5Fixed :: FixedTuple FT5 where
  arrayOf (FT5 a rest) = [a] <> arrayOf rest

instance ft6Fixed :: FixedTuple FT6 where
  arrayOf (FT6 a rest) = [a] <> arrayOf rest

instance ft7Fixed :: FixedTuple FT7 where
  arrayOf (FT7 a rest) = [a] <> arrayOf rest

instance ft8Fixed :: FixedTuple FT8 where
  arrayOf (FT8 a rest) = [a] <> arrayOf rest

instance ft9Fixed :: FixedTuple FT9 where
  arrayOf (FT9 a rest) = [a] <> arrayOf rest

instance ft10Fixed :: FixedTuple FT10 where
  arrayOf (FT10 a rest) = [a] <> arrayOf rest
