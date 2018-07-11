module Example.ExternalComponents.Spec where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, invalid)
import Example.Validation.Semigroup (InvalidPrimitive(..), validateEmailRegex, validateMinimumLength, validateNonEmpty)
import Formless.Spec (FormSpec(..), InputField)
import Formless.Validation (onInputField)

-- | A type synonym for purescript-validation semigroup errors
type Errs = NonEmptyList InvalidPrimitive

-- | Form inputs are expected to have this particular shape and rely
-- | on the `InputField` type from Formless.
newtype Form f = Form
  { name :: f String Errs String
  , email :: f (Maybe String) Errs String
  , whiskey :: f (Maybe String) Errs String
  , language :: f (Maybe String) Errs String
  }
derive instance newtypeForm :: Newtype (Form f) _

-- | You'll usually want symbol proxies for convenience
_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"
_whiskey = SProxy :: SProxy "whiskey"
_language = SProxy :: SProxy "language"

-- | You are meant to provide a wrapper around `FormSpec` in order for
-- | this to all work out. Validators could be written (should be written)
-- | separately.
formSpec :: Form FormSpec
formSpec = Form
  { name: FormSpec ""
  , email: FormSpec Nothing
  , whiskey: FormSpec Nothing
  , language: FormSpec Nothing
  }

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
formValidation :: Form InputField -> Form InputField
formValidation (Form form) = Form
  { name: (\i -> validateNonEmpty i *> validateMinimumLength i 7) `onInputField` form.name
  , email: (\i -> validateMaybe i *> validateEmailRegex (fromMaybe "" i)) `onInputField` form.email
  , whiskey: validateMaybe `onInputField` form.whiskey
  , language: validateMaybe `onInputField` form.language
  }

-- | A custom validator that verifies a value is not Nothing
validateMaybe :: ∀ a. Maybe a -> V Errs a
validateMaybe Nothing = invalid (singleton EmptyField)
validateMaybe (Just a) = pure a

