module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)
import Example.App.Validation as V
import Formless as F

type User = Record (FormRow F.OutputType)

newtype Form r f = Form (r (FormRow f))
derive instance newtypeForm' :: Newtype (Form r f) _

type FormRow f =
  ( name     :: f V.FieldError String         String
  , email    :: f V.FieldError (Maybe String) V.Email
  , whiskey  :: f V.FieldError (Maybe String) String
  , language :: f V.FieldError (Maybe String) String
  )

-- | You'll usually want symbol proxies for convenience
prx :: F.SProxies Form
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy Form

inputs :: Form Record F.InputField
inputs = F.mkInputFields $ F.FormProxy :: F.FormProxy Form

validators :: ∀ m. MonadEffect m => Form Record (F.Validation Form m)
validators = Form
  { name: V.minLength 7
    -- Unpacks the Maybe value, then checks the email format, then verifies it is not in use
    -- monadically.
  , email: V.exists >>> V.emailFormat >>> V.emailIsUsed
  , whiskey: V.exists
  , language: V.exists
  }

submitter :: ∀ m. Monad m => Form Record F.OutputField -> m User
submitter = pure <<< F.unwrapOutputFields
