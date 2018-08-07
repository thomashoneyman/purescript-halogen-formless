module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Example.App.Validation as V
import Formless as F
import Formless.Validation.Semigroup (toEitherPure)

type User = Record (FormRow F.OutputType)

newtype Form r f = Form (r (FormRow f))
derive instance newtypeForm' :: Newtype (Form r f) _

type FormRow f =
  ( name     :: f V.Errs String         String
  , email    :: f V.Errs (Maybe String) String
  , whiskey  :: f V.Errs (Maybe String) String
  , language :: f V.Errs (Maybe String) String
  )

-- | You'll usually want symbol proxies for convenience
prx :: F.SProxies Form
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy Form

inputs :: Form Record F.InputField
inputs = F.mkInputFieldsFromProxy $ F.FormProxy :: F.FormProxy Form

validators :: ∀ m. Monad m => F.PublicState Form m -> Form Record (F.Validator m)
validators _ = F.mkValidators
  { name: toEitherPure $ flip V.validateMinimumLength 7
  , email: toEitherPure $ V.validateEmailRegex <<< fromMaybe ""
  , whiskey: toEitherPure V.validateMaybe
  , language: toEitherPure V.validateMaybe
  }

submitter :: ∀ m. Monad m => Form Record F.OutputField -> m User
submitter = pure <<< F.unwrapOutput
