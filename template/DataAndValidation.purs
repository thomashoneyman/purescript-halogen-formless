module Template.DataAndValidation where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (preview)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.String.CodeUnits (length)
import Formless (FormFieldResult, Validation, _Error, hoistFnE_)

-- Data type for one of our fields

newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

type NAME_FIELD f r = (name :: f FieldError String String | r)

_name :: SProxy "name"
_name = SProxy

-- Error type for one of our fields
data FieldError
  = TooShort Int

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

-- | Function for validating one of our field's data
minLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String String
minLength n = hoistFnE_ $ \str ->
  let n' = length str
   in if n' < n then Left (TooShort n) else Right str

-- | This could be a type class, but we'll just use a function instead.
toErrorText :: FieldError -> String
toErrorText (TooShort n) = "You must enter at least " <> show n <> " characters."

-- | Unpacks errors to render as a string
showError :: ∀ o. FormFieldResult FieldError o -> Maybe String
showError = map toErrorText <<< preview _Error
