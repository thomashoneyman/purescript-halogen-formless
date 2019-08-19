module Template.DataAndValidation where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (preview)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (length)
import Data.Symbol (SProxy(..))
import Formless (class Initial, FormFieldResult, Validation, _Error, hoistFnE_)

-- Data type for one of our fields

newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

                                          -- input  output
type NAME_FIELD f r = ( name :: f FieldError String Name | r)

-- The value following 'SProxy' must be the same as the
-- label in the above row kind, so that we can use `_name`
-- to refer to same label in a record or Variant
_name :: SProxy "name"
_name = SProxy

-- Specifies the initial value that the field will have when it is initialized.
instance initialName :: Initial Name where
  initial = Name "Nobody"

-- Error type for one of our fields
data FieldError
  = TooShort Int

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show x = genericShow x

-- | Function for validating one of our field's data
minLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String Name
minLength n = hoistFnE_ $ \str ->
  let n' = length str
  in if n' < n then Left (TooShort n) else Right (Name str)

-- | This could be a type class, but we'll just use a function instead.
toErrorText :: FieldError -> String
toErrorText (TooShort n) = "You must enter at least " <> show n <> " characters."

-- | Unpacks errors to render as a string
showError :: ∀ o. FormFieldResult FieldError o -> Maybe String
showError = map toErrorText <<< preview _Error
