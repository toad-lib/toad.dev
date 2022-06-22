module Kwap.Route where

import Prelude

import Kwap.Expand (Expand(..))

newtype RouteId = RouteId Int

newtype RouteTitle = RouteTitle String

newtype RoutePathSegment = RoutePathSegment String

newtype RouteConfig = RouteConfig
  { id :: RouteId
  , path :: RoutePathSegment
  , title :: RouteTitle
  , children :: Array RouteConfig
  }

data RouteAttrs = RouteAttrs RouteId RoutePathSegment RouteTitle

data RouteState = RouteState Expand

data Route = Route RouteState RouteAttrs (Array Route)

defaultState :: RouteState
defaultState = RouteState Collapsed

route :: RouteConfig -> Route
route (RouteConfig { id, path, title, children }) = Route defaultState
  (RouteAttrs id path title)
  (route <$> children)

routeAttrs :: Route -> RouteAttrs
routeAttrs (Route _ a _) = a

routeAttrsId :: RouteAttrs -> RouteId
routeAttrsId (RouteAttrs i _ _) = i

routeAttrsPath :: RouteAttrs -> RoutePathSegment
routeAttrsPath (RouteAttrs _ p _) = p

routeAttrsTitle :: RouteAttrs -> RouteTitle
routeAttrsTitle (RouteAttrs _ _ t) = t

routeId :: Route -> RouteId
routeId = routeAttrs >>> routeAttrsId

routePathSegment :: Route -> RoutePathSegment
routePathSegment = routeAttrs >>> routeAttrsPath

routeTitle :: Route -> RouteTitle
routeTitle = routeAttrs >>> routeAttrsTitle
