module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.Utils as V
import Formless.Spec (FormSpec, Input, InputField, Output, OutputField)
import Formless.Spec.Transform (mkFormSpecFromRow, unwrapOutput)
import Formless.Validation.Semigroup (applyOnInputFields)
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
  ( name     :: f V.Errs String         String
  , email    :: f V.Errs (Maybe String) String
  , whiskey  :: f V.Errs (Maybe String) String
  , language :: f V.Errs (Maybe String) String
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
formSpec :: Form FormSpec
formSpec = mkFormSpecFromRow $ RProxy :: RProxy (FormRow Input)

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
validator :: Form InputField -> Form InputField
validator = applyOnInputFields
    { name: flip V.validateMinimumLength 7
    , email: V.validateEmailRegex <<< fromMaybe ""
    , whiskey: \(i :: Maybe String) -> V.validateMaybe i
    , language: \(i :: Maybe String) -> V.validateMaybe i
    }

-- | You should provide a function from the form with only output values to your ideal
-- | parsed type. Since your output type is identical to the form's shape, you can simply
-- | unwrap the form with a helper from Formless.
submitter :: ∀ m. Monad m => Form OutputField -> m User
submitter = pure <<< unwrapOutput
