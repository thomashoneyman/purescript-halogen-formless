module Formless.Record where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Spec (FormSpec(..), InputField(..), MaybeOutput(..), OutputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

-----
-- Functions

-- | A helper function that will run and apply all validation functions to current
-- | inputs to produce the same record, this time with results.
validateInputFields
  :: ∀ row xs form
   . RL.RowToList row xs
  => ValidateInputFields xs row () row
  => Newtype (form InputField) (Record row)
  => form InputField
  -> form InputField
validateInputFields r = wrap $ Builder.build builder {}
  where
    builder = validateInputFieldsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of InputField(s).
formSpecToInputFields
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => FormSpecToInputField xs row () row'
  => Newtype (form FormSpec) (Record row)
  => Newtype (form InputField) (Record row')
  => form FormSpec
  -> form InputField
formSpecToInputFields r = wrap $ Builder.build builder {}
  where
    builder = formSpecToInputFieldBuilder (RLProxy :: RLProxy xs) (unwrap r)

inputFieldToMaybeOutput
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => InputFieldToMaybeOutput xs row () row'
  => Newtype (form InputField) (Record row)
  => Newtype (form MaybeOutput) (Record row')
  => form InputField
  -> form MaybeOutput
inputFieldToMaybeOutput r = wrap $ Builder.build builder {}
  where
    builder = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy xs) (unwrap r)

maybeOutputToOutputField
  :: ∀ i e o row xs row' form
   . RL.RowToList row xs
  => MaybeOutputToOutputField xs row () row'
  => Newtype (MaybeOutput i e o) (Maybe o)
  => Newtype (form MaybeOutput) (Record row)
  => Newtype (form OutputField) (Record row')
  => form MaybeOutput
  -> Maybe (form OutputField)
maybeOutputToOutputField r = map wrap $ Builder.build <@> {} <$> builder
  where
    builder = maybeOutputToOutputFieldBuilder (RLProxy :: RLProxy xs) (unwrap r)


-----
-- Classes (Internal)

-- | The class that provides the Builder implementation to efficiently apply validation
-- | to inputs and produce results
class ValidateInputFields
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  validateInputFieldsBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance validateInputFieldsNil :: ValidateInputFields RL.Nil row () () where
  validateInputFieldsBuilder _ _ = identity

instance validateInputFieldsCons
  :: ( IsSymbol name
     , Row.Cons name (InputField i e o) trash row
     , ValidateInputFields tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (InputField i e o) from' to
     )
  => ValidateInputFields (RL.Cons name (InputField i e o) tail) row from to where
  validateInputFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = validateInputFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField { input, touched, validator }) = InputField
        { input
        , touched
        , validator
        , result: Just $ validator input
        }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of InputField.
class FormSpecToInputField
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  formSpecToInputFieldBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

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
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = formSpecToInputFieldBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormSpec { input, validator }) = InputField
        { input
        , touched: false
        , validator
        , result: Nothing
        }

-- | Transform to the intermediate state, MaybeOutput
class InputFieldToMaybeOutput
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  inputFieldToMaybeOutputBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance inputFieldToMaybeOutputNil :: InputFieldToMaybeOutput RL.Nil row () () where
  inputFieldToMaybeOutputBuilder _ _ = identity

instance inputFieldToMaybeOutputCons
  :: ( IsSymbol name
     , Row.Cons name (InputField i e o) trash row
     , InputFieldToMaybeOutput tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (MaybeOutput i e o) from' to
     )
  => InputFieldToMaybeOutput (RL.Cons name (InputField i e o) tail) row from to where
  inputFieldToMaybeOutputBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField { result }) = MaybeOutput
        case result of
          Just (Right v) -> Just v
          _ -> Nothing


-- | Transform to the intermediate state, MaybeOutput
class MaybeOutputToOutputField
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  maybeOutputToOutputFieldBuilder :: RLProxy xs -> Record row -> Maybe (Builder { | from } { | to })

instance maybeOutputToOutputFieldNil :: MaybeOutputToOutputField RL.Nil row () () where
  maybeOutputToOutputFieldBuilder _ _ = Just identity

instance maybeOutputToOutputFieldCons
  :: ( IsSymbol name
     , Newtype (MaybeOutput i e o) (Maybe o)
     , Row.Cons name (MaybeOutput i e o) trash row
     , MaybeOutputToOutputField tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (OutputField i e o) from' to
     )
  => MaybeOutputToOutputField (RL.Cons name (MaybeOutput i e o) tail) row from to where
  maybeOutputToOutputFieldBuilder _ r =
    transform <$> val <*> rest
    where
      _name = SProxy :: SProxy name

      val :: Maybe (OutputField i e o)
      val = map OutputField $ unwrap $ Record.get _name r

      rest :: Maybe (Builder { | from } { | from' })
      rest = maybeOutputToOutputFieldBuilder (RLProxy :: RLProxy tail) r

      transform
        :: OutputField i e o
        -> Builder { | from } { | from' }
        -> Builder { | from } { | to }
      transform v builder' = Builder.insert _name v <<< builder'
