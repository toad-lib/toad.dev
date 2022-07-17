module Kwap.Error
  ( Error
  , ErrorMessage(..)
  , fetchingManifest
  , lookingUpRouteConcept
  , parsingConcept
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (error)
import Halogen (HalogenM(..))

newtype ErrorMessage = ErrorMessage String

derive newtype instance showErrorMessage :: Show ErrorMessage
derive newtype instance eqErrorMessage :: Eq ErrorMessage

type Error = Tuple String ErrorMessage

addHumanErrorMessage :: String -> String -> Error
addHumanErrorMessage h i = Tuple i (ErrorMessage h)

fetchingManifest :: String -> Error
fetchingManifest = addHumanErrorMessage
  "An error occurred fetching concepts. Please file an issue on GitHub!"

lookingUpRouteConcept :: String -> Error
lookingUpRouteConcept = addHumanErrorMessage
  "Couldn't find Concept in URL! This is probably not a bug."

parsingConcept :: String -> Error
parsingConcept = addHumanErrorMessage
  "Failed to parse this concept. Please file an issue in GitHub!"
