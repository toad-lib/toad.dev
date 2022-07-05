module Kwap.Concept (Decl(..), fetchDecl) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Core (Json, jsonNull) as Text.Json
import Data.Argonaut.Parser (jsonParser) as Text.Json
import Data.Bifunctor (lmap, rmap)
import Data.Codec.Argonaut ((>~>))
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , decode
  , printJsonDecodeError
  , string
  ) as Text.Json
import Data.Codec.Argonaut.Migration (addDefaultField) as Text.Json
import Data.Codec.Argonaut.Compat (maybe) as Text.Json
import Data.Codec.Argonaut.Record (object) as Text.Json
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP

data Decl = Decl
  ( Array
      { path :: String
      , title :: String
      , alias :: Maybe String
      }
  )

derive instance genericDecl :: Generic Decl _
instance showDecl :: Show Decl where show = genericShow

declCodec
  :: Text.Json.JsonCodec
       (Array { path :: String, title :: String, alias :: Maybe String })
declCodec = Text.Json.array
          $ Text.Json.addDefaultField "alias" Text.Json.jsonNull
          >~> Text.Json.object "Decl"
              { path: Text.Json.string
              , title: Text.Json.string
              , alias: Text.Json.maybe Text.Json.string
              }

decodeDecl :: String -> Either String Decl
decodeDecl s = do
  json <- Text.Json.jsonParser s
  decls <- lmap Text.Json.printJsonDecodeError $ Text.Json.decode
    declCodec
    json
  pure $ Decl decls

baseUrl :: String
baseUrl =
  "https://raw.githubusercontent.com/clov-cofee/kwap-docs/main/concepts"

fetchDecl :: Aff (Either String Decl)
fetchDecl =
  HTTP.fetch (HTTP.URL $ baseUrl <> "/index.json") HTTP.Get mempty
    >>= HTTP.text
    <#> decodeDecl
