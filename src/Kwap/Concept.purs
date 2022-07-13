module Kwap.Concept (Manifest, Decl, decls, fetchManifest, pathString, aliasString, titleString, path, alias, title, Path(..), Alias(..), Title(..)) where

import Prelude

import Data.Argonaut.Core (jsonNull) as Text.Json
import Data.Argonaut.Parser (jsonParser) as Text.Json
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut ((>~>))
import Data.Codec.Argonaut (JsonCodec, array, decode, printJsonDecodeError, string) as Text.Json
import Data.Codec.Argonaut.Compat (maybe) as Text.Json
import Data.Codec.Argonaut.Migration (addDefaultField) as Text.Json
import Data.Codec.Argonaut.Record (object) as Text.Json
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Fetch as HTTP

newtype Alias = Alias String
derive newtype instance showAlias :: Show Alias
derive newtype instance eqAlias :: Eq Alias
derive newtype instance ordAlias :: Ord Alias
derive newtype instance semiAlias :: Semigroup Alias
derive newtype instance monoidAlias :: Monoid Alias

aliasString :: Alias -> String
aliasString (Alias s) = s

newtype Path = Path String
derive newtype instance showPath :: Show Path
derive newtype instance eqPath :: Eq Path
derive newtype instance ordPath :: Ord Path
derive newtype instance semiPath :: Semigroup Path
derive newtype instance monoidPath :: Monoid Path

pathString :: Path -> String
pathString (Path s) = s

newtype Title = Title String
derive newtype instance showTitle :: Show Title
derive newtype instance eqTitle :: Eq Title
derive newtype instance ordTitle :: Ord Title
derive newtype instance semiTitle :: Semigroup Title
derive newtype instance monoidTitle :: Monoid Title

titleString :: Title -> String
titleString (Title s) = s

newtype Decl = Decl
  { path :: Path
  , title :: Title
  , alias :: Maybe Alias
  }

derive newtype instance eqOne :: Eq Decl
derive newtype instance showOne :: Show Decl

declOfRecord :: {path :: String, title :: String, alias :: Maybe String} -> Decl
declOfRecord {path: p, title: t, alias: a} = Decl {path: Path p, title: Title t, alias: Alias <$> a}

path :: Decl -> Path
path (Decl { path: p }) = p

title :: Decl -> Title
title (Decl { title: t }) = t

alias :: Decl -> Alias
alias (Decl { path: (Path p), alias: a }) = fromMaybe (Alias p) a

newtype Manifest = Manifest (Array Decl)

derive newtype instance eqManifest :: Eq Manifest
derive newtype instance showManifest :: Show Manifest

decls :: Manifest -> Array Decl
decls (Manifest a) = a

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

fetchManifest :: HTTP.FetchImpl -> Aff (Either String Manifest)
fetchManifest impl =
  HTTP.fetch impl (HTTP.URL $ baseUrl <> "/index.json") HTTP.Get mempty
    >>= HTTP.text
    <#> decodeManifest
