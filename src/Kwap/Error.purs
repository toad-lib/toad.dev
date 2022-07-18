module Kwap.Error
  ( Error
  , ErrorMessage(..)
  , fetchingManifest
  , lookingUpRouteConcept
  , parsingConcept
  ) where

import Prelude

import Data.Tuple (Tuple(..))

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
  "Concept not found!"

parsingConcept :: String -> Error
parsingConcept = addHumanErrorMessage
  "Failed to parse this concept. Please file an issue in GitHub!"
