module Kwap.App.Html (module X, withText, h1, h2, h3, h4, h5, h6, p) where

import Kwap.App.Css
import Prelude

import Data.Array as Array
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML hiding (h1, h2, h3, h4, h5, h6, p) as X
import Halogen.HTML.CSS (style)

withText :: ∀ a w i. (Array (HTML w i) -> a) -> String -> a
withText ctor =
  HH.text
    >>> Array.singleton
    >>> ctor

textStyle :: CSS
textStyle = color (Purple Darkest)

headingStyle :: forall i r. Font -> IProp (style :: String | r) i
headingStyle font' =
  style do
    textStyle
    font font'
    margin nil nil nil nil

h1 :: ∀ w i. Array (HTML w i) -> HTML w i
h1 = HH.h1 [ headingStyle $ fontSize FontSizeH1 <> fontFamily StokeBold ]

h2 :: ∀ w i. Array (HTML w i) -> HTML w i
h2 = HH.h2 [ headingStyle $ fontSize FontSizeH2 <> fontFamily StokeBold ]

h3 :: ∀ w i. Array (HTML w i) -> HTML w i
h3 = HH.h3 [ headingStyle $ fontSize FontSizeH3 <> fontFamily StokeBold ]

h4 :: ∀ w i. Array (HTML w i) -> HTML w i
h4 = HH.h4 [ headingStyle $ fontSize FontSizeH4 <> fontFamily InterBold ]

h5 :: ∀ w i. Array (HTML w i) -> HTML w i
h5 = HH.h5 [ headingStyle $ fontSize FontSizeH5 <> fontFamily InterBold ]

h6 :: ∀ w i. Array (HTML w i) -> HTML w i
h6 = HH.h6 [ headingStyle $ fontSize FontSizeH6 <> fontFamily InterBold ]

p :: ∀ w i. Array (HTML w i) -> HTML w i
p =
  HH.p
    [ style do
        textStyle
        font mempty
        margin nil nil nil nil
    ]
