module Formless.Spec where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))

-- Have the user pass a row of types like this, from which I can derive
-- all the information I need? How do I get the validation functions?

--  type Inputs f =
--    ( name :: f String (Array String) String
--    , email :: f String (Array String) String
--    )

type InputField i e o =
  { input :: i
  , touched :: Boolean
  , validator :: i -> Either e o
  , result :: Maybe (Either e o)
  }

_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_validator = SProxy :: SProxy "validator"
_result = SProxy :: SProxy "result"
