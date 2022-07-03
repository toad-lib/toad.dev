module Kwap.App where

import Prelude

import Kwap.App.Action (Action(..))
import Kwap.App.Content as App.Content
import Kwap.App.Css as Css
import Kwap.App.Html as HH
import Kwap.App.Layout (AppLayout(..))
import Kwap.App.Navbar as App.Navbar
import Kwap.App.State as App.State
import Kwap.App.Style as App.Style
import Kwap.App.Style.Global as App.Style.Global

render :: forall w. App.State.State -> HH.HTML w Action
render state =
  HH.div_
    [ App.Style.Global.stylesheet
    , HH.div
        [ Css.style $ App.Style.appBackground $ App.State.kwapGradient state
        ]
        []
    , HH.div
        [ Css.style App.Style.appWrap
        ]
        [ HH.div
            [ Css.style App.Style.navbarWrap
            ]
            [ App.Navbar.render NavbarSectionPicked AppLayoutDesktop
                (App.State.navbarSection state)
            ]
        , HH.div
            [ Css.style App.Style.contentWrap
            ]
            [ App.Content.render
            ]
        ]
    ]
