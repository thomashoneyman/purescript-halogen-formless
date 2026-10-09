-- | This module demonstrates how to write simple validation functions for your
-- | forms. You can write validation functions of any kind so long as they
-- | ultimately result in an `Either` value.
module Example.Utils.Validation where

import Prelude

import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String as String
import Example.Utils.Types (Email(..), Username(..))

requiredText :: String -> Either String String
requiredText input
  | input == "" = Left "Required."
  | otherwise = Right input

shorterThan :: Int -> String -> Either String String
shorterThan limit str
  | String.length str >= limit = Left $ "Must be shorter than " <> show limit <> " characters."
  | otherwise = Right str

longerThan :: Int -> String -> Either String String
longerThan limit str
  | String.length str <= limit = Left $ "Must be longer than " <> show limit <> " characters."
  | otherwise = Right str

email :: String -> Either String Email
email str
  | not (String.contains (Pattern "@") str) = Left "Must contain the @ character."
  | not (String.contains (Pattern ".") str) = Left "Must end in a top-level domain like '.com'."
  | String.length str <= 5 = Left "Not a valid email address."
  | otherwise = Right $ Email str

username :: String -> Either String Username
username = map Username <<< (longerThan 5 <=< requiredText)
