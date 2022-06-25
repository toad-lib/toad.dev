module Kwap.App.Logo (render) where

import Kwap.App.Css
import Prelude hiding (bottom, top)

import CSS.Common as Css.Common
import Kwap.App.Html as HH

foreign import kwapMaskUrl :: String

maskImageRatio :: Number
maskImageRatio = 5.0 / 12.0

maskWidthRem :: Number
maskWidthRem = 8.0

maskHeightRem :: Number
maskHeightRem = maskWidthRem * maskImageRatio

paddingRem :: Number
paddingRem = 1.0

overlap :: Number
overlap = 0.3

maskOutlineInnerWidthRem :: Number
maskOutlineInnerWidthRem = maskWidthRem - overlap

maskOutlineInnerHeightRem :: Number
maskOutlineInnerHeightRem = maskHeightRem - overlap

render :: ∀ w i. HH.HTML w i
render = HH.div
  [ style do
      position relative
      display flex
      justifyContent Css.Common.center
      alignItems Css.Common.center
      height $ rem maskHeightRem
      width $ rem maskWidthRem
      sym padding $ rem paddingRem
  ]
  [ HH.div
      [ style do
          position absolute
          height $ rem maskOutlineInnerHeightRem
          width $ rem maskOutlineInnerWidthRem
          top $ rem (overlap / 2.0)
          left $ rem (overlap / 2.0)
          border solid (rem paddingRem) (cssColor $ Yellow Lightest)
          sym borderRadius $ rem 0.5
      ]
      []
  , HH.div
      [ style do
          width $ rem maskWidthRem
          height $ rem maskHeightRem
          backgroundColor $ Yellow Lightest
          mask
            [ Css.Common.other $ fromString
                "linear-gradient(black, black)"
            , url kwapMaskUrl
            ]
            MaskExclude
            MaskAlpha
            ( ( Css.Common.other $ fromString "center"
              ) :: Side
            )
            (rem maskWidthRem)
      ]
      []
  ]
