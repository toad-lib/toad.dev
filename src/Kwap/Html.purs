module Kwap.Html
  ( module X
  , headingStyle
  , withText
  , classNames
  , ClassProp
  , a
  , a_
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
  , p_
  ) where

import Kwap.Css hiding (map)
import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (ClassName(..))
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML hiding (a, a_, h1_, h2_, h3_, h4_, h5_, h6_, map, p_) as X
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Kwap.Route as Route

type ClassProp i r = HP.IProp (class :: String | r) i

classNames :: forall i r. Array String -> ClassProp i r
classNames = (map ClassName) >>> HP.classes

withText :: ∀ a w i. (Array (HTML w i) -> a) -> String -> a
withText ctor =
  HH.text
    >>> Array.singleton
    >>> ctor

headingStyleProp :: forall i r. Font -> IProp (style :: String | r) i
headingStyleProp = headingStyle >>> style

headingStyle :: forall i r. Font -> CSS
headingStyle font' = do
  font font'
  margin nil nil nil nil

a
  :: forall r w i
   . Array (IProp I.HTMLa i)
  -> Route.Route
  -> Array (HTML w i)
  -> HTML w i
a ps r t = HH.a
  (ps <> [ HP.href <<< append "#" $ Route.print r ])
  t

a_ :: forall w i. Route.Route -> Array (HTML w i) -> HTML w i
a_ r t = HH.a [ HP.href <<< append "#" $ Route.print r ] t

h1_ :: ∀ w i. Array (HTML w i) -> HTML w i
h1_ = HH.h1
  [ headingStyleProp $ fontSize FontSizeH1 <> fontFamily InterExtraBold ]

h2_ :: ∀ w i. Array (HTML w i) -> HTML w i
h2_ = HH.h2 [ headingStyleProp $ fontSize FontSizeH2 <> fontFamily InterBold ]

h3_ :: ∀ w i. Array (HTML w i) -> HTML w i
h3_ = HH.h3 [ headingStyleProp $ fontSize FontSizeH3 <> fontFamily InterBold ]

h4_ :: ∀ w i. Array (HTML w i) -> HTML w i
h4_ = HH.h4 [ headingStyleProp $ fontSize FontSizeH4 <> fontFamily InterBold ]

h5_ :: ∀ w i. Array (HTML w i) -> HTML w i
h5_ = HH.h5 [ headingStyleProp $ fontSize FontSizeH5 <> fontFamily InterBold ]

h6_ :: ∀ w i. Array (HTML w i) -> HTML w i
h6_ = HH.h6 [ headingStyleProp $ fontSize FontSizeH6 <> fontFamily InterBold ]

p_ :: ∀ w i. Array (HTML w i) -> HTML w i
p_ =
  HH.p
    [ style do
        font mempty
        margin nil nil nil nil
    ]
