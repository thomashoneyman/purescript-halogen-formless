module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.Validation.Semigroup as V
import Formless.Spec (Output, OutputField, unwrapOutput)
import Formless.Spec as FSpec
import Formless.Validation (onInputField)
import Type.Row (RProxy(..))

-- | You must provide a newtype around your form record in this format. Here,
-- | rather than a record accepting `f`, we're just providing a row.
newtype Form f = Form (Record (FormRow f))
derive instance newtypeForm :: Newtype (Form f) _

-- | This is the actual type you want to parse to and use throughout your program.
-- | In this case, it'll be the exact record output by the form, but in many cases,
-- | it may be another shape.
type User = Record (FormRow Output)

-- | We'll use this row to generate our form spec, but also to represent the
-- | available fields in the record.
type FormRow f =
  ( name     :: f String         V.Errs String
  , email    :: f (Maybe String) V.Errs String
  , whiskey  :: f (Maybe String) V.Errs String
  , language :: f (Maybe String) V.Errs String
  )

-- | You'll usually want symbol proxies for convenience
_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"
_whiskey = SProxy :: SProxy "whiskey"
_language = SProxy :: SProxy "language"

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything. This is useful for especially
-- | large forms where you don't want to have to stick newtypes everywhere.
-- | If you already have a record of values, use `mkFormSpec` instead.
formSpec :: Form FSpec.FormSpec
formSpec = FSpec.mkFormSpecFromRow $ RProxy :: RProxy (FormRow FSpec.Input)

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
formValidation :: Form FSpec.InputField -> Form FSpec.InputField
formValidation (Form form) = Form
  { name: (\i -> V.validateNonEmpty i *> V.validateMinimumLength i 7) `onInputField` form.name
  , email: (\i -> V.validateMaybe i *> V.validateEmailRegex (fromMaybe "" i)) `onInputField` form.email
  , whiskey: V.validateMaybe `onInputField` form.whiskey
  , language: V.validateMaybe `onInputField` form.language
  }

-- | You should provide a function from the form with only output values to your ideal
-- | parsed type. Since your output type is identical to the form's shape, you can simply
-- | unwrap the form with a helper from Formless.
outputParser :: Form OutputField -> User
outputParser = unwrapOutput
