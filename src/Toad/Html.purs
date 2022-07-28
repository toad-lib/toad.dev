module Toad.Html
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
  , h1Font
  , h2Font
  , h3Font
  , h4Font
  , h5Font
  , h6Font
  , p_
  , span_
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (ClassName(..))
import Halogen.HTML
  ( class IsProp
  , AttrName(..)
  , ClassName(..)
  , ComponentHTML
  , ElemName(..)
  , HTML(..)
  , IProp
  , Leaf
  , Namespace(..)
  , Node
  , PlainHTML
  , PropName(..)
  , abbr
  , abbr_
  , address
  , address_
  , area
  , article
  , article_
  , aside
  , aside_
  , attr
  , attrNS
  , audio
  , audio_
  , b
  , b_
  , base
  , bdi
  , bdi_
  , bdo
  , bdo_
  , blockquote
  , blockquote_
  , body
  , body_
  , br
  , br_
  , button
  , button_
  , canvas
  , caption
  , caption_
  , cite
  , cite_
  , code
  , code_
  , col
  , colgroup
  , colgroup_
  , command
  , datalist
  , datalist_
  , dd
  , dd_
  , del
  , del_
  , details
  , details_
  , dfn
  , dfn_
  , dialog
  , dialog_
  , div
  , div_
  , dl
  , dl_
  , dt
  , dt_
  , element
  , elementNS
  , em
  , em_
  , embed
  , embed_
  , fieldset
  , fieldset_
  , figcaption
  , figcaption_
  , figure
  , figure_
  , footer
  , footer_
  , form
  , form_
  , fromPlainHTML
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , handler
  , head
  , head_
  , header
  , header_
  , hr
  , hr_
  , html
  , html_
  , i
  , i_
  , iframe
  , img
  , input
  , ins
  , ins_
  , kbd
  , kbd_
  , keyed
  , keyedNS
  , label
  , label_
  , lazy
  , lazy2
  , lazy3
  , legend
  , legend_
  , li
  , li_
  , link
  , main
  , main_
  , map_
  , mark
  , mark_
  , memoized
  , menu
  , menu_
  , menuitem
  , menuitem_
  , meta
  , meter
  , meter_
  , nav
  , nav_
  , noscript
  , noscript_
  , object
  , object_
  , ol
  , ol_
  , optgroup
  , optgroup_
  , option
  , option_
  , output
  , output_
  , p
  , param
  , pre
  , pre_
  , progress
  , progress_
  , prop
  , q
  , q_
  , rp
  , rp_
  , rt
  , rt_
  , ruby
  , ruby_
  , samp
  , samp_
  , script
  , script_
  , section
  , section_
  , select
  , select_
  , slot
  , slot_
  , small
  , small_
  , source
  , span
  , strong
  , strong_
  , style
  , style_
  , sub
  , sub_
  , summary
  , summary_
  , sup
  , sup_
  , table
  , table_
  , tbody
  , tbody_
  , td
  , td_
  , text
  , textarea
  , tfoot
  , tfoot_
  , th
  , th_
  , thead
  , thead_
  , time
  , time_
  , title
  , title_
  , tr
  , tr_
  , track
  , u
  , u_
  , ul
  , ul_
  , var
  , var_
  , video
  , video_
  , wbr
  , withKeys
  , withKeys_
  ) as X
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Toad.Css
  ( CSS
  , Font
  , FontFamily(..)
  , FontSize(..)
  , font
  , fontFamily
  , fontSize
  , margin
  , nil
  )
import Toad.Route as Route

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

h1Font :: Font
h1Font = fontSize FontSizeH1 <> fontFamily AbrilFatface

h2Font :: Font
h2Font = fontSize FontSizeH2 <> fontFamily LibreBaskervilleBold

h3Font :: Font
h3Font = fontSize FontSizeH3 <> fontFamily LibreBaskervilleBold

h4Font :: Font
h4Font = fontSize FontSizeH4 <> fontFamily LibreBaskervilleItalic

h5Font :: Font
h5Font = fontSize FontSizeH5 <> fontFamily LibreBaskervilleItalic

h6Font :: Font
h6Font = fontSize FontSizeH6 <> fontFamily AtkinsonBold

h1_ :: ∀ w i. Array (HTML w i) -> HTML w i
h1_ = HH.h1 [ headingStyleProp h1Font ]

h2_ :: ∀ w i. Array (HTML w i) -> HTML w i
h2_ = HH.h2 [ headingStyleProp h2Font ]

h3_ :: ∀ w i. Array (HTML w i) -> HTML w i
h3_ = HH.h3 [ headingStyleProp h3Font ]

h4_ :: ∀ w i. Array (HTML w i) -> HTML w i
h4_ = HH.h4 [ headingStyleProp h4Font ]

h5_ :: ∀ w i. Array (HTML w i) -> HTML w i
h5_ = HH.h5 [ headingStyleProp h5Font ]

h6_ :: ∀ w i. Array (HTML w i) -> HTML w i
h6_ = HH.h6 [ headingStyleProp h6Font ]

p_ :: ∀ w i. Array (HTML w i) -> HTML w i
p_ =
  HH.p
    [ style do
        font mempty
        margin nil nil nil nil
    ]

span_ :: ∀ w i. Array (HTML w i) -> HTML w i
span_ = HH.span [ style $ font mempty ]
