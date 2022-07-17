module Kwap.Concept.Fetch (manifest, one) where

import Prelude

import Data.DateTime (DateTime(..), Month(..), Year, canonicalDate, diff, year)
import Data.Either (Either)
import Data.Enum (fromEnum, toEnum)
import Data.Int (floor)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Kwap.Concept (Manifest, Path, decodeManifest, pathString)

baseUrl :: String
baseUrl =
  "https://raw.githubusercontent.com/clov-coffee/kwap-docs/main/"

manifestUrl :: String
manifestUrl =
  baseUrl <> "concepts.json"

maybeEpoch :: Maybe DateTime
maybeEpoch = flip DateTime bottom <$>
  (pure canonicalDate <*> toEnum 1970 <*> pure January <*> toEnum 1)

unixTime :: DateTime -> Seconds
unixTime = maybe (const $ Seconds 0.0) (flip diff) maybeEpoch

cacheBust :: DateTime -> HTTP.URL -> HTTP.URL
cacheBust dt (HTTP.URL u) =
  HTTP.URL
    $ u
        <> "?cache-bust="
        <> (show <<< floor <<< unwrap <<< unixTime $ dt)

manifest :: HTTP.FetchImpl -> Aff (Either String Manifest)
manifest impl =
  let
    url = HTTP.URL manifestUrl
  in
    do
      url' <- pure cacheBust <*> liftEffect nowDateTime <*> pure url
      res <- HTTP.fetch impl url' HTTP.Get mempty
      decodeManifest <$> HTTP.text res

one :: HTTP.FetchImpl -> Path -> Aff String
one impl p =
  let
    url = HTTP.URL $ baseUrl <> "concepts/" <> pathString p
  in
    do
      url' <- pure cacheBust <*> liftEffect nowDateTime <*> pure url
      res <- HTTP.fetch impl url' HTTP.Get mempty
      HTTP.text res
