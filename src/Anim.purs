module Anim where

import Halogen.HTML.Core as HC

data Fade
  = Out
  | In

wipeUpClass :: Fade -> HC.ClassName
wipeUpClass Out = HC.ClassName "wipe-up-out"

wipeUpClass In = HC.ClassName "wipe-up-in"

fadeClass :: Fade -> HC.ClassName
fadeClass Out = HC.ClassName "fade-out"

fadeClass In = HC.ClassName "fade-in"
