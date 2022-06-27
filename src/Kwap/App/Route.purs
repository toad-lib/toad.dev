module Kwap.App.Route where

import Kwap.Route
import Prelude

root :: Route
root =
  RouteConfig
    { id: RouteId 0
    , path: RoutePathSegment "/"
    , title: RouteTitle "kwap"
    , children: []
    }
    # route
