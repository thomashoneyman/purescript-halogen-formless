module Formless.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Spec (FormSpec(..), InputField(..), OutputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

-----
-- Types

-- | Never exposed to the user, but used to
-- | aid transformations
newtype MaybeOutput i e o = MaybeOutput (Maybe o)
derive instance newtypeMaybeOutput :: Newtype (MaybeOutput i e o) _

-- | Never exposed to the user, but used to aid equality instances for
-- | checking dirty states.
newtype Input i e o = Input i
derive instance newtypeInput :: Newtype (Input i e o) _
derive newtype instance eqInput :: Eq i => Eq (Input i e o)


-----
-- Functions

-- | A helper function that will count all errors in a record
countErrors
  :: ∀ form row xs row' xs'
   . RL.RowToList row xs
  => RL.RowToList row' xs'
  => CountErrors xs row () row'
  => SumRecord xs' row' (Additive Int)
  => Newtype (form InputField) (Record row)
  => form InputField
  -> Int
countErrors = unwrap <<< sumRecord <<< countErrors' <<< unwrap

-- | A helper function that sums a monoidal record
sumRecord
  :: ∀ r rl a
   . SumRecord rl r a
  => RL.RowToList r rl
  => Monoid a
  => Record r
  -> a
sumRecord r = sumImpl (RLProxy :: RLProxy rl) r

-- | A helper function that will set all input fields to 'touched = true'. This ensures
-- | subsequent validations apply to all fields even if not edited by the user.
setInputFieldsTouched
  :: ∀ row xs form
   . RL.RowToList row xs
  => SetInputFieldsTouched xs row () row
  => Newtype (form InputField) (Record row)
  => form InputField
  -> form InputField
setInputFieldsTouched r = wrap $ Builder.build builder {}
  where
    builder = setInputFieldsTouchedBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of InputField(s) into
-- | just the input value
inputFieldsToInput
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => InputFieldsToInput xs row () row'
  => Newtype (form InputField) (Record row)
  => Newtype (form Input) (Record row')
  => form InputField
  -> form Input
inputFieldsToInput r = wrap $ Builder.build builder {}
  where
    builder = inputFieldsToInputBuilder (RLProxy :: RLProxy xs) (unwrap r)

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

-- | An intermediate function that transforms a record of InputField into a record
-- | of MaybeOutput as a step in producing output fields.
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

-- | A function that, when used with `inputFieldToMaybeOutput`, turns a record of
-- | InputField into a record of OutputField if all fields in the record are successfully
-- | validated.
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

-- | A class to set all input fields to touched for validation purposes
class SetInputFieldsTouched
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  setInputFieldsTouchedBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance setInputFieldsTouchedNil :: SetInputFieldsTouched RL.Nil row () () where
  setInputFieldsTouchedBuilder _ _ = identity

instance setInputFieldsTouchedCons
  :: ( IsSymbol name
     , Row.Cons name (InputField i e o) trash row
     , SetInputFieldsTouched tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (InputField i e o) from' to
     )
  => SetInputFieldsTouched (RL.Cons name (InputField i e o) tail) row from to where
  setInputFieldsTouchedBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = setInputFieldsTouchedBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField i) = InputField i { touched = true }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of InputField.
class InputFieldsToInput
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  inputFieldsToInputBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance inputFieldsToInputNil :: InputFieldsToInput RL.Nil row () () where
  inputFieldsToInputBuilder _ _ = identity

instance inputFieldsToInputCons
  :: ( IsSymbol name
     , Row.Cons name (InputField i e o) trash row
     , InputFieldsToInput tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (Input i e o) from' to
     )
  => InputFieldsToInput (RL.Cons name (InputField i e o) tail) row from to where
  inputFieldsToInputBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = inputFieldsToInputBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField fields) = Input fields.input

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
      transform (FormSpec input) = InputField
        { input
        , touched: false
        , result: Nothing
        }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of InputField to record of MaybeOutput.
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


-- | The class that provides the Builder implementation to efficiently transform the record
-- | of MaybeOutput to a record of OutputField, but only if all fields were successfully
-- | validated.
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


-- | A class to sum a monoidal record
class SumRecord (rl :: RL.RowList) (r :: # Type) a | rl -> a where
  sumImpl :: RLProxy rl -> Record r -> a

instance nilSumRecord :: Monoid a => SumRecord RL.Nil r a where
  sumImpl _ _ = mempty

instance consSumRecord
  :: ( IsSymbol name
     , Monoid a
     , Row.Cons name a t0 r
     , SumRecord tail r a
     )
  => SumRecord (RL.Cons name a tail) r a
  where
    sumImpl _ r =
      -- This has to be defined in a variable for some reason; it won't
      -- compile otherwise, but I don't know why not.
      let tail' = sumImpl (RLProxy :: RLProxy tail) r
          val = Record.get (SProxy :: SProxy name) r
       in val <> tail'

-- | Gets out ints
class CountErrors
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  countErrorsBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance countErrorsNil :: CountErrors RL.Nil row () () where
  countErrorsBuilder _ _ = identity

instance countErrorsCons
  :: ( IsSymbol name
     , Row.Cons name (InputField i e o) trash row
     , CountErrors tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (Additive Int) from' to
     )
  => CountErrors (RL.Cons name (InputField i e o) tail) row from to where
  countErrorsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = countErrorsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField { result }) =
        case result of
          Just (Left _) -> Additive 1
          _ -> Additive 0

-- Intermediate function for countErrors
countErrors'
  :: ∀ row xs row'
   . RL.RowToList row xs
  => CountErrors xs row () row'
  => Record row
  -> Record row'
countErrors' r = Builder.build builder {}
  where
    builder = countErrorsBuilder (RLProxy :: RLProxy xs) r

