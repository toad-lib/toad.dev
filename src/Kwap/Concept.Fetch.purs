module Kwap.Concept.Fetch ( manifest ) where

import Prelude

import Data.Argonaut.Core (jsonNull) as Text.Json
import Data.Argonaut.Parser (jsonParser) as Text.Json
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , decode
  , printJsonDecodeError
  , string
  ) as Text.Json
import Data.Codec.Argonaut ((>~>))
import Data.Codec.Argonaut.Compat (maybe) as Text.Json
import Data.Codec.Argonaut.Migration (addDefaultField) as Text.Json
import Data.Codec.Argonaut.Record (object) as Text.Json
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP
import Kwap.Concept (declOfRecord, Manifest(..))

manifestCodec
  :: Text.Json.JsonCodec
       (Array { path :: String, title :: String, alias :: Maybe String })
manifestCodec = Text.Json.array
  $ Text.Json.addDefaultField "alias" Text.Json.jsonNull
      >~> Text.Json.object "Manifest"
        { path: Text.Json.string
        , title: Text.Json.string
        , alias: Text.Json.maybe Text.Json.string
        }

decodeManifest :: String -> Either String Manifest
decodeManifest s = do
  json <- Text.Json.jsonParser s
  decls' <- lmap Text.Json.printJsonDecodeError $ Text.Json.decode
    manifestCodec
    json
  pure $ Manifest $ declOfRecord <$> decls'

baseUrl :: String
baseUrl =
  "https://raw.githubusercontent.com/clov-coffee/kwap-docs/main/concepts"

manifest :: HTTP.FetchImpl -> Aff (Either String Manifest)
manifest impl =
  HTTP.fetch impl (HTTP.URL $ baseUrl <> "/index.json") HTTP.Get mempty
    >>= HTTP.text
    <#> decodeManifest

