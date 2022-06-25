module Kwap.App.Logo (render) where

import Kwap.App.Css
import Prelude hiding (top, bottom)

import CSS.Common as Css.Common
import Kwap.App.Html as HH

foreign import kwapMaskUrl :: String

maskImageRatio :: Number
maskImageRatio = 500.0 / 1200.0

maskWidthRem :: Number
maskWidthRem = 8.0

maskHeightRem :: Number
maskHeightRem = maskWidthRem * maskImageRatio

render :: ∀ w i. HH.HTML w i
render = HH.div
  [style do
           position relative
           display flex
           justifyContent Css.Common.center
           alignItems Css.Common.center
           height $ rem maskHeightRem
           width $ rem maskWidthRem
           sym padding $ rem 1.0
  ]
  [ HH.div [style do
             position absolute
             height $ rem (maskHeightRem - 0.025)
             width $ rem (maskWidthRem - 0.025)
             border solid (rem 1.0) (cssColor $ Yellow Lightest)
             sym borderRadius $ rem 0.5
             top $ rem 0.025
             left $ rem 0.025
           ] []
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
