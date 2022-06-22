module Kwap.Route.Routes where

import Kwap.Route
import Prelude

root :: Route
root =
  RouteConfig
    { id: RouteId 0
    , path: RoutePathSegment "/"
    , title: RouteTitle "kwap"
    , children:
        [ RouteConfig
            { id: RouteId 1
            , path: RoutePathSegment "hello"
            , title: RouteTitle "hello"
            , children: []
            }
        ]
    }
    # route
