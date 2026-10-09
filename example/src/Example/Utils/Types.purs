module Example.Utils.Types where

import Prelude

import Data.Newtype (class Newtype)

data Picked = One | Two | Three

derive instance Eq Picked

instance Show Picked where
  show = case _ of
    One -> "One"
    Two -> "Two"
    Three -> "Three"

newtype Username = Username String

derive instance Newtype Username _

instance Show Username where
  show (Username username) = "(Username " <> username <> ")"

newtype Email = Email String

derive instance Newtype Email _

instance Show Email where
  show (Email email) = "(Email " <> email <> ")"
