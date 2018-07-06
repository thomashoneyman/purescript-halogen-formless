module Formless.Spec where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

-- An example input type that should be provided:
--  type Form f =
--    ( name :: f String (Array String) String
--    , email :: f String (Array String) String
--    )

newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean
  , validator :: input -> Either error output
  , result :: Maybe (Either error output)
  }
derive instance newtypeInputField :: Newtype (InputField i e o) _

newtype OutputField input error output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField i e o) _

_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_validator = SProxy :: SProxy "validator"
_result = SProxy :: SProxy "result"
