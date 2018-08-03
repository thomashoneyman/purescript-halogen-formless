module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Example.App.Validation as V
import Formless.Spec (FormProxy(..), FormSpec(..), OutputField, OutputType)
import Formless.Spec.Transform (SProxies, mkSProxies, unwrapOutput)
import Formless.Validation.Semigroup (toEitherPure)

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
prx :: SProxies Form
prx = mkSProxies $ FormProxy :: FormProxy Form

formSpec :: ∀ m. Monad m => Form Record (FormSpec m)
formSpec = Form
  { name: FormSpec
    { input: ""
    , validator: toEitherPure $ flip V.validateMinimumLength 7
    }
  , email: FormSpec
    { input: Nothing
    , validator: toEitherPure $ V.validateEmailRegex <<< fromMaybe ""
    }
  , whiskey: FormSpec
    { input: Nothing
    , validator: toEitherPure V.validateMaybe
    }
  , language: FormSpec
    { input: Nothing
    , validator: toEitherPure V.validateMaybe
    }
  }

submitter :: ∀ m. Monad m => Form Record OutputField -> m User
submitter = pure <<< unwrapOutput
