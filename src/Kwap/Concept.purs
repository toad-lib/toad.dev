module Kwap.Concept (Decl, One, array, fetchDecl, path, alias, title) where

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
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP

newtype One = One
  { path :: String
  , title :: String
  , alias :: Maybe String
  }

derive newtype instance eqOne :: Eq One
derive newtype instance showOne :: Show One

path :: One -> String
path (One { path: p }) = p

title :: One -> String
title (One { title: t }) = t

alias :: One -> Maybe String
alias (One { alias: a }) = a

newtype Decl = Decl (Array One)

derive newtype instance eqDecl :: Eq Decl
derive newtype instance showDecl :: Show Decl

array :: Decl -> Array One
array (Decl a) = a

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
  pure $ Decl $ One <$> decls

baseUrl :: String
baseUrl =
  "https://raw.githubusercontent.com/clov-coffee/kwap-docs/main/concepts"

fetchDecl :: Aff (Either String Decl)
fetchDecl =
  HTTP.fetch (HTTP.URL $ baseUrl <> "/index.json") HTTP.Get mempty
    >>= HTTP.text
    <#> decodeDecl
