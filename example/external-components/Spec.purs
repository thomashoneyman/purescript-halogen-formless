module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.App.Validation as V
import Formless.Spec (FormSpec, InputType, InputField, OutputType, OutputField)
import Formless.Spec.Transform (mkFormSpecFromRow, unwrapOutput)
import Formless.Validation.Semigroup (applyOnInputFields)
import Type.Row (RProxy(..))

type User = Record (FormRow OutputType)

newtype Form f = Form (Record (FormRow f))
derive instance newtypeForm :: Newtype (Form f) _

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

formSpec :: Form FormSpec
formSpec = mkFormSpecFromRow $ RProxy :: RProxy (FormRow InputType)

validator :: Form InputField -> Form InputField
validator = applyOnInputFields
    { name: flip V.validateMinimumLength 7
    , email: V.validateEmailRegex <<< fromMaybe ""
    , whiskey: \(i :: Maybe String) -> V.validateMaybe i
    , language: \(i :: Maybe String) -> V.validateMaybe i
    }

submitter :: ∀ m. Monad m => Form OutputField -> m User
submitter = pure <<< unwrapOutput
