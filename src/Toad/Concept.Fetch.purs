module Toad.Concept.Fetch (manifest, one, humanUrl) where

import Toad.Prelude

import Data.DateTime (DateTime(..), Month(..), canonicalDate, diff)
import Data.Enum (toEnum)
import Data.Int (floor)
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Toad.Concept (Manifest, Path, decodeManifest, pathString)

rawContentBaseUrl :: String
rawContentBaseUrl =
  "https://raw.githubusercontent.com/clov-coffee/toad.dev-docs/main/"

humanBaseUrl :: String
humanBaseUrl = "https://github.com/clov-coffee/toad.dev-docs/tree/main/"

manifestUrl :: String
manifestUrl = rawContentBaseUrl <> "concepts.json"

maybeEpoch :: Maybe DateTime
maybeEpoch = flip DateTime bottom <$>
  (pure canonicalDate <*> toEnum 1970 <*> pure January <*> toEnum 1)

unixTime :: DateTime -> Seconds
unixTime = maybe (const $ Seconds 0.0) (flip diff) maybeEpoch

cacheBust :: DateTime -> HTTP.URL -> HTTP.URL
cacheBust dt (HTTP.URL u) = HTTP.URL $ u <> "?cache-bust=" <>
  (show ∘ floor ∘ unwrap ∘ unixTime $ dt)

rawContentUrl :: Path -> HTTP.URL
rawContentUrl p = HTTP.URL $ rawContentBaseUrl <> "concepts/" <> pathString p

humanUrl :: Path -> HTTP.URL
humanUrl p = HTTP.URL $ humanBaseUrl <> "concepts/" <> pathString p

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
  do
    url' <- pure cacheBust <*> liftEffect nowDateTime <*> pure (rawContentUrl p)
    res <- HTTP.fetch impl url' HTTP.Get mempty
    HTTP.text res
