module Formless.Spec where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

-- An example input type that should be provided:
--
--  newtype Form f = Form
--    { name :: f String (Array String) String
--    , email :: f String (Array String) String
--    }
--
-- form :: Form FormSpec
-- form = Form
--   { name ::
--       { input: ""
--       , validator: \str -> if length str > 8 then Left "String not long enough" else Right str
--       }
--   , email ::
--       { input: ""
--       , validator: \str -> if String.contains (Pattern "@") str then Right str else Left "Not a valid email."
--       }
--   }

-- | The type that will be applied to the user's input row to
-- | create the spec form that we'll compare against to measure
-- | 'touched' states, etc. This is what the user is responsible
-- | for providing.
newtype FormSpec input error output = FormSpec
  { input :: input
  , validator :: input -> Either error output
  }

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean
  , validator :: input -> Either error output
  , result :: Maybe (Either error output)
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

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of InputField(s).
formSpecToInputField
  :: ∀ row xs row' spec fields
   . RL.RowToList row xs
  => FormSpecToInputField xs row () row'
  => Newtype spec (Record row)
  => Newtype fields (Record row')
  => spec
  -> fields
formSpecToInputField r = wrap $ Builder.build builder {}
  where
    builder = formSpecToInputFieldBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of InputField.
class FormSpecToInputField (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> row from to where
  formSpecToInputFieldBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

-- | Instances are only for RowList and RowCons
instance formSpecToInputFieldNil :: FormSpecToInputField RL.Nil row () () where
  formSpecToInputFieldBuilder _ _ = identity

instance formSpecToInputFieldCons
  :: ( IsSymbol name
     , Row.Cons name (FormSpec i e o) trash row
     , FormSpecToInputField tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (InputField i e o) from' to
     )
  => FormSpecToInputField (RL.Cons name (FormSpec i e o) tail) row from to where
  formSpecToInputFieldBuilder _ r =
    first <<< rest
    where
      nameP = SProxy :: SProxy name
      val = transform $ Record.get nameP r
      rest = formSpecToInputFieldBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert nameP val
      transform (FormSpec { input, validator }) = InputField
        { input
        , touched: false
        , validator
        , result: Nothing
        }
