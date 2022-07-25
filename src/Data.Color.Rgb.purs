module Data.Color.Rgb where

import Prelude

newtype R = R Number
newtype G = G Number
newtype B = B Number

data Rgb = Rgb R G B
