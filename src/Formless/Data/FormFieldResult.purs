module Formless.Data.FormFieldResult where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))

-- | A data type which represents the possible output states of the field. Use
-- | the helpers in `Retrieve` to easily manipulate this type.
data FormFieldResult error output
  = NotValidated
  | Validating
  | Error error
  | Success output

derive instance genericFormFieldResult :: Generic (FormFieldResult e o) _
derive instance eqFormFieldResult :: (Eq e, Eq o) => Eq (FormFieldResult e o)
derive instance functorFormFieldResult :: Functor (FormFieldResult e)

instance applyFormFieldResult :: Apply (FormFieldResult e) where
  apply (Success f) r = map f r
  apply (Error e) _ = Error e
  apply Validating _ = Validating
  apply NotValidated _ = NotValidated

instance applicativeFormFieldResult :: Applicative (FormFieldResult e) where
  pure = Success

instance bindFormFieldResult :: Bind (FormFieldResult e) where
  bind (Success a) f = f a
  bind (Error e) _ = Error e
  bind Validating _ = Validating
  bind NotValidated _ = NotValidated

instance monadFormFieldResult :: Monad (FormFieldResult e)

instance showFormFieldResult :: (Show e, Show o) => Show (FormFieldResult e o) where
  show = genericShow

fromEither :: forall e o. Either e o -> FormFieldResult e o
fromEither = case _ of
  Left e -> Error e
  Right v -> Success v

toMaybe :: forall e o. FormFieldResult e o -> Maybe o
toMaybe = case _ of
  Success v -> Just v
  _ -> Nothing

equalsWith
  :: forall e o
   . (e -> e -> Boolean)
  -> (o -> o -> Boolean)
  -> (FormFieldResult e o -> FormFieldResult e o -> Boolean)
equalsWith errorEquals outputEquals = case _, _ of
  Success l, Success r -> l `outputEquals` r
  Error l, Error r -> l `errorEquals` r
  Validating, Validating -> true
  NotValidated, NotValidated -> true
  _, _ -> false

_Error :: forall e o. Prism' (FormFieldResult e o) e
_Error = prism' Error case _ of
  Error e -> Just e
  _ -> Nothing

_Success :: forall e o. Prism' (FormFieldResult e o) o
_Success = prism' Success case _ of
  Success o -> Just o
  _ -> Nothing
