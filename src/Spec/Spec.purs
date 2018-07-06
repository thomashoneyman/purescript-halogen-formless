module Formless.Spec where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))

-- An example input type that should be provided

--  type Form = Record (Form' InputField)
--  type Form' f =
--    ( name :: f String (Array String) String
--    , email :: f String (Array String) String
--    )

type InputField input error output =
  { input :: input
  , touched :: Boolean
  , validator :: input -> Either error output
  , result :: Maybe (Either error output)
  }

_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_validator = SProxy :: SProxy "validator"
_result = SProxy :: SProxy "result"
