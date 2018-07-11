module Example.ExternalComponents.Spec where

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Formless.Spec (FormSpec(..))

-- | Form inputs are expected to have this particular shape and rely
-- | on the `InputField` type from Formless.
newtype Form f = Form
  { name :: f String String String
  , email :: f (Maybe String) String String
  , whiskey :: f (Maybe String) String String
  , language :: f (Maybe String) String String
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
