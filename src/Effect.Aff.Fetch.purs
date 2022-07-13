module Effect.Aff.Fetch
  ( module X
  , fetch
  , Body(..)
  , opt
  , Options
  , Credentials(..)
  , Redirect(..)
  , Method(..)
  , class Opt
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Yoga.Fetch
  ( Response
  , URL(..)
  , arrayBuffer
  , headers
  , json
  , statusCode
  , text
  , url
  ) as X
import Yoga.Fetch as Fetch
import Yoga.Fetch.Impl (FetchImpl)
import Yoga.Fetch.Impl (FetchImpl) as X

foreign import optionsToFetchOptions
  :: ∀ a
   . (Maybe a -> Boolean)
  -> (Maybe a -> a)
  -> { body :: Maybe String
     , credentials :: Maybe Fetch.Credentials
     , redirect :: Maybe Fetch.Redirect
     , headers :: Maybe (Array { key :: String, value :: String })
     , method :: Fetch.Method
     }
  -> { method :: Fetch.Method | Fetch.Options }

fetch
  :: FetchImpl
  -> Fetch.URL
  -> Method
  -> Options
  -> Aff Fetch.Response
fetch impl url method opts = Fetch.fetch impl url (yogaOptions method opts)

data Credentials = OmitCredentials | IncludeCredentials | SameOriginCredentials

yogaCredentials :: Credentials -> Fetch.Credentials
yogaCredentials = case _ of
  OmitCredentials -> Fetch.omitCredentials
  IncludeCredentials -> Fetch.includeCredentials
  SameOriginCredentials -> Fetch.sameOriginCredentials

data Redirect = ErrorOnRedirect | FollowRedirects | NoFollowRedirects

yogaRedirect :: Redirect -> Fetch.Redirect
yogaRedirect = case _ of
  ErrorOnRedirect -> Fetch.redirectError
  FollowRedirects -> Fetch.redirectFollow
  NoFollowRedirects -> Fetch.redirectManual

data Method = Get | Put | Post | Delete

yogaMethod :: Method -> Fetch.Method
yogaMethod = case _ of
  Get -> Fetch.getMethod
  Put -> Fetch.putMethod
  Post -> Fetch.postMethod
  Delete -> Fetch.deleteMethod

newtype Body = Body String

unwrapBody :: Body -> String
unwrapBody (Body s) = s

class Opt a where
  opt :: a -> Options

modify
  :: (OptionsRecord -> OptionsRecord) -> Options -> Options
modify f (Options r) = Options $ f r

instance bodyOpt :: Opt Body where
  opt body = modify (_ { body = Just body }) mempty

instance headersOpt :: Opt (Map.Map String String) where
  opt headers = modify (_ { headers = Just headers }) mempty

instance redirectOpt :: Opt Redirect where
  opt redirect = modify (_ { redirect = Just redirect }) mempty

instance credsOpt :: Opt Credentials where
  opt credentials = modify (_ { credentials = Just credentials }) mempty

type OptionsRecord =
  { body :: Maybe Body
  , credentials :: Maybe Credentials
  , redirect :: Maybe Redirect
  , headers :: Maybe (Map.Map String String)
  }

data Options = Options OptionsRecord

yogaOptions
  :: Method
  -> Options
  -> { method :: Fetch.Method | Fetch.Options }
yogaOptions method (Options { body, credentials, redirect, headers }) =
  optionsToFetchOptions
    isJust
    (unsafePartial fromJust)
    { body: unwrapBody <$> body
    , credentials: yogaCredentials <$> credentials
    , redirect: yogaRedirect <$> redirect
    , headers:
        ( \headers' -> (\(Tuple k v) -> { key: k, value: v }) <$>
            (Map.toUnfoldableUnordered headers' :: Array _)
        ) <$> headers
    , method: yogaMethod method
    }

instance semiOptions :: Semigroup (Options) where
  append
    (Options { body: bA, credentials: cA, redirect: rA, headers: hA })
    (Options { body: bB, credentials: cB, redirect: rB, headers: hB }) =
    Options
      { body: bB <|> bA
      , credentials: cB <|> cA
      , redirect: rB <|> rA
      , headers: hB <|> hA
      }

instance monoidOptions :: Monoid (Options) where
  mempty = Options
    { body: Nothing, credentials: Nothing, redirect: Nothing, headers: Nothing }
