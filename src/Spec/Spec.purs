module Formless.Spec where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

-- | The type that will be applied to the user's input row to
-- | create the spec form that we'll compare against to measure
-- | 'touched' states, etc. This is what the user is responsible
-- | for providing.
newtype FormSpec input error output = FormSpec input
derive instance newtypeFormSpec :: Newtype (FormSpec i e o) _

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output) -- force the user to end up in Either?
  }
derive instance newtypeInputField :: Newtype (InputField i e o) _

-- | Proxies for each of the fields in InputField and FormSpec for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_validator = SProxy :: SProxy "validator"
_result = SProxy :: SProxy "result"

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField input error output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField i e o) _

-- | Perhaps temporary; never exposed to the user, but used to
-- | aid transformations
newtype MaybeOutput i e o = MaybeOutput (Maybe o)
derive instance newtypeMaybeOutput :: Newtype (MaybeOutput i e o) _

