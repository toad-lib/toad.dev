module Kwap.Concept.Fetch (manifest) where

import Prelude

import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP
import Kwap.Concept (Manifest, decodeManifest)

manifestUrl :: String
manifestUrl =
  "https://raw.githubusercontent.com/clov-coffee/kwap-docs/main/concepts.json"

manifest :: HTTP.FetchImpl -> Aff (Either String Manifest)
manifest impl =
  HTTP.fetch impl (HTTP.URL manifestUrl) HTTP.Get mempty
    >>= HTTP.text
    <#> decodeManifest
