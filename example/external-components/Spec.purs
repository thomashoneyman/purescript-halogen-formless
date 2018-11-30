module Example.ExternalComponents.Spec where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Example.App.Validation as V
import Formless as F

-- We can easily reclaim our ideal User type by picking the
-- parsed outputs from the FormRow like this:
type User = Record (UserFormRow F.OutputType)

newtype UserForm r f = UserForm (r (UserFormRow f))
derive instance newtypeUserForm' :: Newtype (UserForm r f) _

type UserFormRow f =
  ( name     :: f V.FieldError String         String
  , email    :: f V.FieldError (Maybe String) V.Email
  , whiskey  :: f V.FieldError (Maybe String) String
  , language :: f V.FieldError (Maybe String) String
  )

-- | You'll usually want symbol proxies for convenience
prx :: F.SProxies UserForm
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy UserForm

-- | You can generate your initial inputs
initialInputs :: UserForm Record F.InputField
initialInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy UserForm

validators :: ∀ m. Monad m => UserForm Record (F.Validation UserForm m)
validators = UserForm
  { name: V.minLength 7
    -- Unpacks the Maybe value, then checks the email format
  , email: V.exists >>> V.emailFormat
  , whiskey: V.exists
  , language: V.exists
  }
