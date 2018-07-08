module Example.ExternalComponents.Spec where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Symbol (SProxy(..))
import Formless.Spec (FormSpec(..))

-- | Form inputs are expected to have this particular shape and rely
-- | on the `InputField` type from Formless.
newtype Form f = Form
  { name :: f String String String
  , email :: f String String String
  }
derive instance newtypeForm :: Newtype (Form f) _

-- | You'll usually want symbol proxies for convenience
_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"

-- | You are meant to provide a wrapper around `FormSpec` in order for
-- | this to all work out. Validators could be written (should be written)
-- | separately.
formSpec :: Form FormSpec
formSpec = Form
  { name: FormSpec
      { input: ""
      , validator: \str ->
          if String.length str < 3
            then Left "Must be 3 characters or more."
            else Right str
      }
  , email: FormSpec
      { input: ""
      , validator: \str ->
          if String.contains (String.Pattern "_") str
            then Right str
            else Left "Email addresses must have underscores."
      }
  }
