module Formless.Spec where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)


-- Our form spec is required to deliver to the form component, which
-- will build out the other fields that it needs from this.
type Errors = Array String

newtype Name = Name String
newtype Email = Email String

newtype ExampleFormSpec = ExampleFormSpec
  { inputs ::
      { name :: String
      , email :: String
      }
  , validators ::
      { name :: String -> Either Errors Name
      , email :: String -> Either Errors Email
      }
  }
derive instance newtypeFormSpec :: Newtype ExampleFormSpec _

----------
-- Scratchpad

-- Have the user pass a row of types like this, from which I can derive
-- all the information I need? How do I get the validation functions?
type Inputs f =
  { name :: f String Errors Name
  , email :: f String Errors Email
  }


-- to create field inputs
type MkInput i e o = i
type MkTouched i e o = Boolean
type MkValidator i e o = (i -> Either e o)
type MkResult i e o = Maybe (Either e o)

type InputForm =
  { input :: Inputs MkInput
  , touched :: Inputs MkTouched
  , validator :: Inputs MkValidator
  , result :: Inputs MkResult
  }
