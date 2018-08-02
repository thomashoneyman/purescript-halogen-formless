module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Example.App.Validation as V
import Formless.Spec (FormProxy(..), FormSpec, FormInput, OutputField, OutputType)
import Formless.Spec.Transform (SProxies, mkFormSpecFromProxy, mkSProxies, unwrapOutput)
import Formless.Validation.Semigroup (applyOnFormInputs)

type User = Record (FormRow OutputType)

newtype Form r f = Form (r (FormRow f))
derive instance newtypeForm' :: Newtype (Form r f) _

type FormRow f =
  ( name     :: f V.Errs String         String
  , email    :: f V.Errs (Maybe String) String
  , whiskey  :: f V.Errs (Maybe String) String
  , language :: f V.Errs (Maybe String) String
  )

-- | You'll usually want symbol proxies for convenience
proxies :: SProxies Form
proxies = mkSProxies $ FormProxy :: FormProxy Form

formSpec :: Form Record FormSpec
formSpec = mkFormSpecFromProxy $ FormProxy :: FormProxy Form

validator :: Form Record FormInput -> Form Record FormInput
validator = applyOnFormInputs
    { name: flip V.validateMinimumLength 7
    , email: V.validateEmailRegex <<< fromMaybe ""
    , whiskey: \(i :: Maybe String) -> V.validateMaybe i
    , language: \(i :: Maybe String) -> V.validateMaybe i
    }

submitter :: ∀ m. Monad m => Form Record OutputField -> m User
submitter = pure <<< unwrapOutput
