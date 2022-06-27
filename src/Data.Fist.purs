module Data.Fist where

import Prelude

--| Fixed-Capacity List
class Fist t where
  arrayOf :: ∀ a. t a -> Array a

data Fist1 a =  Fist1 a
data Fist2 a =  Fist2 a (Fist1 a)
data Fist3 a =  Fist3 a (Fist2 a)
data Fist4 a =  Fist4 a (Fist3 a)
data Fist5 a =  Fist5 a (Fist4 a)
data Fist6 a =  Fist6 a (Fist5 a)
data Fist7 a =  Fist7 a (Fist6 a)
data Fist8 a =  Fist8 a (Fist7 a)
data Fist9 a =  Fist9 a (Fist8 a)
data Fist10 a = Fist10 a (Fist9 a)

fist1 :: ∀ a. a -> Fist1 a
fist1 = Fist1

fist2 :: ∀ a. a -> a -> Fist2 a
fist2 a = fist1 >>> Fist2 a

fist3 :: ∀ a. a -> a -> a -> Fist3 a
fist3 a b = fist2 b >>> Fist3 a

fist4 :: ∀ a. a -> a -> a -> a -> Fist4 a
fist4 a b c = fist3 b c >>> Fist4 a

fist5 :: ∀ a. a -> a -> a -> a -> a -> Fist5 a
fist5 a b c d = fist4 b c d >>> Fist5 a

fist6 :: ∀ a. a -> a -> a -> a -> a -> a -> Fist6 a
fist6 a b c d e = fist5 b c d e >>> Fist6 a

fist7 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> Fist7 a
fist7 a b c d e f = fist6 b c d e f >>> Fist7 a

fist8 :: ∀ a. a -> a -> a -> a -> a-> a  -> a -> a -> Fist8 a
fist8 a b c d  e f g= fist7 b c d e f g >>> Fist8 a

fist9 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> Fist9 a
fist9 a b c d e f g h = fist8 b c d e f g h >>> Fist9 a

fist10 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Fist10 a
fist10 a b c d e f g h i = fist9 b c d e f g h i >>> Fist10 a

instance ft1Fixed :: Fist Fist1 where
  arrayOf (Fist1 a) = [a]

instance ft2Fixed :: Fist Fist2 where
  arrayOf (Fist2 a rest) = [a] <> arrayOf rest

instance ft3Fixed :: Fist Fist3 where
  arrayOf (Fist3 a rest) = [a] <> arrayOf rest

instance ft4Fixed :: Fist Fist4 where
  arrayOf (Fist4 a rest) = [a] <> arrayOf rest

instance ft5Fixed :: Fist Fist5 where
  arrayOf (Fist5 a rest) = [a] <> arrayOf rest

instance ft6Fixed :: Fist Fist6 where
  arrayOf (Fist6 a rest) = [a] <> arrayOf rest

instance ft7Fixed :: Fist Fist7 where
  arrayOf (Fist7 a rest) = [a] <> arrayOf rest

instance ft8Fixed :: Fist Fist8 where
  arrayOf (Fist8 a rest) = [a] <> arrayOf rest

instance ft9Fixed :: Fist Fist9 where
  arrayOf (Fist9 a rest) = [a] <> arrayOf rest

instance ft10Fixed :: Fist Fist10 where
  arrayOf (Fist10 a rest) = [a] <> arrayOf rest
