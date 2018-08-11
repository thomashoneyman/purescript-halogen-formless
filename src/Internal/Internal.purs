module Formless.Internal where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, on)
import Formless.Spec (FormField(..), InputField(..), OutputField(..), FormFieldRow)
import Formless.Validation (Validation)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

-----
-- Types

-- | Represents empty input
data U e i o = U

-- | Represents building some output record from an empty record
type FromScratch r = Builder {} (Record r)

-- | Apply a builder that produces an output record from an empty record
fromScratch :: ∀ r. FromScratch r -> Record r
fromScratch = Builder.build <@> {}

-- | A constraint synonym for Row.Cons and Row.Lacks
class (Row.Cons s t r r', Row.Lacks s r) <= Row1Cons s t r r' | s t r -> r', s r' -> t r
instance row1Cons :: (Row.Cons s t r r', Row.Lacks s r) => Row1Cons s t r r'

-----
-- Functions

-- | A helper function that will count all errors in a record
checkTouched
  :: ∀ form fields xs
   . RL.RowToList fields xs
  => AllTouched xs fields
  => Newtype (form Record FormField) (Record fields)
  => form Record FormField
  -> Boolean
checkTouched = allTouchedImpl (RLProxy :: RLProxy xs) <<< unwrap

-- | A helper function that will count all errors in a record
countErrors
  :: ∀ form fs xs row fields
   . RL.RowToList row xs
  => RL.RowToList fields fs
  => CountErrors fs fields row
  => SumRecord xs row (Additive Int)
  => Newtype (form Record FormField) (Record fields)
  => form Record FormField
  -> Int
countErrors r = unwrap $ sumRecord $ fromScratch builder
  where builder = countErrorsBuilder (RLProxy :: RLProxy fs) (unwrap r)

-- | A helper function that sums a monoidal record
sumRecord
  :: ∀ r rl a
   . SumRecord rl r a
  => RL.RowToList r rl
  => Record r
  -> a
sumRecord = sumImpl (RLProxy :: RLProxy rl)

-- | A helper function that will automatically transform a record of FormField(s) into
-- | just the input value
formFieldsToInputFields
  :: ∀ xs form fields inputs
   . RL.RowToList fields xs
  => FormFieldsToInputFields xs fields inputs
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record FormField) (Record fields)
  => form Record FormField
  -> form Record InputField
formFieldsToInputFields r = wrap $ fromScratch builder
  where builder = formFieldsToInputFieldsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of FormField(s).
inputFieldsToFormFields
  :: ∀ xs form inputs fields
   . RL.RowToList inputs xs
  => InputFieldsToFormFields xs inputs fields
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record FormField) (Record fields)
  => form Record InputField
  -> form Record FormField
inputFieldsToFormFields r = wrap $ fromScratch builder
  where builder = inputFieldsToFormFieldsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | An intermediate function that transforms a record of FormField into a record
inputFieldToMaybeOutput
  :: ∀ xs form fields outputs
   . RL.RowToList fields xs
  => Newtype (form Record FormField) (Record fields)
  => Newtype (form Record OutputField) (Record outputs)
  => FormFieldToMaybeOutput xs fields outputs
  => form Record FormField
  -> Maybe (form Record OutputField)
inputFieldToMaybeOutput r = map wrap $ fromScratch <$> builder
  where builder = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | Transform form fields, with pure results
transformFormFields
  :: ∀ xs form fields
   . RL.RowToList fields xs
  => TransformFormFields xs fields fields
  => Newtype (form Record FormField) (Record fields)
  => (∀ e i o. FormField e i o -> FormField e i o)
  -> form Record FormField
  -> form Record FormField
transformFormFields f r = wrap $ fromScratch builder
  where builder = transformFormFieldsBuilder f (RLProxy :: RLProxy xs) (unwrap r)

replaceFormFieldInputs
  :: ∀ xs form fields inputs
   . RL.RowToList fields xs
  => ReplaceFormFieldInputs inputs xs fields fields
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record FormField) (Record fields)
  => form Record InputField
  -> form Record FormField
  -> form Record FormField
replaceFormFieldInputs inputs fields = wrap $ fromScratch builder
  where builder = replaceFormFieldInputsBuilder (unwrap inputs) (RLProxy :: RLProxy xs) (unwrap fields)

applyValidation
  :: ∀ st vs xs form fields m
   . RL.RowToList fields xs
  => Monad m
  => ApplyValidation st vs xs fields fields m
  => Newtype (form Record (Validation st m)) (Record vs)
  => Newtype (form Record FormField) (Record fields)
  => st
  -> form Record (Validation st m)
  -> form Record FormField
  -> m (form Record FormField)
applyValidation st vs fs = map wrap $ fromScratch <$> builder
  where
    builder = applyValidationBuilder st (unwrap vs) (RLProxy :: RLProxy xs) (unwrap fs)

-----
-- Classes (Internal)

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of FormField.
class FormFieldsToInputFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  formFieldsToInputFieldsBuilder :: RLProxy xs -> Record row -> FromScratch to

instance inputFieldsToInputNil :: FormFieldsToInputFields RL.Nil row () where
  formFieldsToInputFieldsBuilder _ _ = identity

instance inputFieldsToInputCons
  :: ( IsSymbol name
     , Row.Cons name (FormField e i o) trash row
     , FormFieldsToInputFields tail row from
     , Row1Cons name (InputField e i o) from to
     )
  => FormFieldsToInputFields (RL.Cons name (FormField e i o) tail) row to where
  formFieldsToInputFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = formFieldsToInputFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormField fields) = InputField fields.input

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of FormField.
class InputFieldsToFormFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  inputFieldsToFormFieldsBuilder :: RLProxy xs -> Record row -> FromScratch to

instance inputFieldsToFormFieldsNil :: InputFieldsToFormFields RL.Nil row () where
  inputFieldsToFormFieldsBuilder _ _ = identity

instance inputFieldsToFormFieldsCons
  :: ( IsSymbol name
     , Row.Cons name (InputField e i o) trash row
     , InputFieldsToFormFields tail row from
     , Row1Cons name (FormField e i o) from to
     )
  => InputFieldsToFormFields (RL.Cons name (InputField e i o) tail) row to where
  inputFieldsToFormFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = inputFieldsToFormFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (InputField input) = FormField
        { input
        , touched: false
        , result: Nothing
        }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of MaybeOutput to a record of OutputField, but only if all fields were successfully
-- | validated.
class FormFieldToMaybeOutput (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  inputFieldToMaybeOutputBuilder :: RLProxy xs -> Record row -> Maybe (FromScratch to)

instance inputFieldToMaybeOutputNil :: FormFieldToMaybeOutput RL.Nil row () where
  inputFieldToMaybeOutputBuilder _ _ = Just identity

instance inputFieldToMaybeOutputCons
  :: ( IsSymbol name
     , Row.Cons name (FormField e i o) trash row
     , FormFieldToMaybeOutput tail row from
     , Row1Cons name (OutputField e i o) from to
     )
  => FormFieldToMaybeOutput (RL.Cons name (FormField e i o) tail) row to where
  inputFieldToMaybeOutputBuilder _ r =
    transform <$> val <*> rest
    where
      _name = SProxy :: SProxy name

      val :: Maybe (OutputField e i o)
      val = map OutputField $ join $ map hush (unwrap $ Record.get _name r).result

      rest :: Maybe (FromScratch from)
      rest = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy tail) r

      transform :: OutputField e i o -> FromScratch from -> FromScratch to
      transform v builder' = Builder.insert _name v <<< builder'

-- | A class to sum a monoidal record
class Monoid a <= SumRecord (rl :: RL.RowList) (r :: # Type) a | rl -> a where
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
class CountErrors (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  countErrorsBuilder :: RLProxy xs -> Record row -> FromScratch to

instance countErrorsNil :: CountErrors RL.Nil row () where
  countErrorsBuilder _ _ = identity

instance countErrorsCons
  :: ( IsSymbol name
     , Row.Cons name (FormField e i o) trash row
     , CountErrors tail row from
     , Row1Cons name (Additive Int) from to
     )
  => CountErrors (RL.Cons name (FormField e i o) tail) row to where
  countErrorsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = countErrorsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormField { result }) =
        case result of
          Just (Left _) -> Additive 1
          _ -> Additive 0

-- | A class to check if all fields in an FormField record have been touched or not
class AllTouched (rl :: RL.RowList) (r :: # Type) where
  allTouchedImpl :: RLProxy rl -> Record r -> Boolean

instance nilAllTouched :: AllTouched RL.Nil r where
  allTouchedImpl _ _ = true

instance consAllTouched
  :: ( IsSymbol name
     , Row.Cons name (FormField e i o) t0 r
     , AllTouched tail r
     )
  => AllTouched (RL.Cons name (FormField e i o) tail) r
  where
    allTouchedImpl _ r =
      if (unwrap $ Record.get (SProxy :: SProxy name) r).touched
        then allTouchedImpl (RLProxy :: RLProxy tail) r
        else false

-- | @monoidmusician
class InputVariant r to where
  setInputVariant
    :: (∀ e i o. InputField e i o -> FormField e i o)
    -> Variant r
    -> Record to
    -> Record to

instance setInputVariant' ::
  ( RL.RowToList r rl
  , SetInputVariantRL rl r to
  ) => InputVariant r to where
    setInputVariant f = setInputVariantRL f (RLProxy :: RLProxy rl)

class SetInputVariantRL rl v to | rl -> v where
  setInputVariantRL
    :: (∀ e i o. InputField e i o -> FormField e i o)
    -> RLProxy rl
    -> Variant v
    -> Record to
    -> Record to

instance setInputVariantNil :: SetInputVariantRL RL.Nil () to where
  setInputVariantRL _ _ = case_

instance setInputVariantCons ::
  ( IsSymbol s
  , SetInputVariantRL rl v to
  , Row.Cons s (FormField e i o) t0 to
  , Row.Cons s (InputField e i o) v v'
  ) => SetInputVariantRL (RL.Cons s (InputField e i o) rl) v' to where
    setInputVariantRL f _ =
      on s f' (setInputVariantRL f (RLProxy :: RLProxy rl))
      where
        s = SProxy :: SProxy s
        f' :: InputField e i o -> { | to } -> { | to }
        f' a r = Record.set s (f a) r

class ValidateVariant r to m where
  validateVariant
    :: Monad m
    => (∀ e i o. FormField e i o -> m (FormField e i o))
    -> Variant r
    -> Record to
    -> m (Record to)

instance validateVariant' ::
  ( RL.RowToList r rl
  , ValidateVariantRL rl r to m
  ) => ValidateVariant r to m where
    validateVariant f = validateVariantRL f (RLProxy :: RLProxy rl)

class ValidateVariantRL rl v to m | rl -> v where
  validateVariantRL
    :: Monad m
    => (∀ e i o. FormField e i o -> m (FormField e i o))
    -> RLProxy rl
    -> Variant v
    -> Record to
    -> m (Record to)

instance validateVariantNil :: ValidateVariantRL RL.Nil () to m where
  validateVariantRL _ _ = case_

instance validateVariantCons ::
  ( IsSymbol s
  , ValidateVariantRL rl v to m
  , Row.Cons s (FormField e i o) t0 to
  , Row.Cons s (U e i o) v v'
  ) => ValidateVariantRL (RL.Cons s (FormField e i o) rl) v' to m where
    validateVariantRL f _ =
      on s f' (validateVariantRL f (RLProxy :: RLProxy rl))
      where
        s = SProxy :: SProxy s

        -- The variant must contain some value of the correct kind, so we'll
        -- use `U` to represent nothing
        f' :: U e i o -> { | to } -> m ({ | to })
        f' _ r = do
          let x = Record.get s r
          x' <- f x
          pure $ Record.set s x' r

----------
-- Transform form fields

class TransformFormFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  transformFormFieldsBuilder :: (∀ e i o. FormField e i o -> FormField e i o) -> RLProxy xs -> Record row -> FromScratch to

instance transformFormFieldsTouchedNil :: TransformFormFields RL.Nil row () where
  transformFormFieldsBuilder _ _ _ = identity

instance transformFormFieldsTouchedCons
  :: ( IsSymbol name
     , Row.Cons name (FormField e i o) trash row
     , Row1Cons name (FormField e i o) from to
     , TransformFormFields tail row from
     )
  => TransformFormFields (RL.Cons name (FormField e i o) tail) row to where
  transformFormFieldsBuilder f _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      first = Builder.insert _name (f (Record.get _name r))
      rest = transformFormFieldsBuilder f (RLProxy :: RLProxy tail) r

-- | A class that applies the current state to the unwrapped version of every validator
class ApplyValidation st (vs :: # Type) (xs :: RL.RowList) (row :: # Type) (to :: # Type) m | xs -> to where
  applyValidationBuilder :: st -> Record vs -> RLProxy xs -> Record row -> m (FromScratch to)

instance applyToValidationNil :: Monad m => ApplyValidation st vs RL.Nil row () m where
  applyValidationBuilder _ _ _ _ = pure identity

instance applyToValidationCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (FormField e i o) t0 row
     , Row.Cons name (Validation st m e i o) t1 vs
     , Row1Cons name (FormField e i o) from to
     , ApplyValidation st vs tail row from m
     )
  => ApplyValidation st vs (RL.Cons name (FormField e i o) tail) row to m where
  applyValidationBuilder st vs _ r =
    fn <$> val <*> rest
    where
      _name = SProxy :: SProxy name

      fn val' rest' = Builder.insert _name val' <<< rest'

      rest = applyValidationBuilder st vs (RLProxy :: RLProxy tail) r

      val = do
        let validator = unwrap $ Record.get _name vs
            formField = unwrap $ Record.get _name r
        res <- validator st formField.input
        pure $ wrap $ formField { result = Just res }


----------
-- Replace form fields

class ReplaceFormFieldInputs (inputs :: # Type) (xs :: RL.RowList) (fields :: # Type) (to :: # Type) | xs -> to where
  replaceFormFieldInputsBuilder ::  Record inputs -> RLProxy xs -> Record fields -> FromScratch to

instance replaceFormFieldInputsTouchedNil :: ReplaceFormFieldInputs inputs RL.Nil fields () where
  replaceFormFieldInputsBuilder _ _ _ = identity

instance replaceFormFieldInputsTouchedCons
  :: ( IsSymbol name
     , Newtype (InputField e i o) i
     , Newtype (FormField e i o) (Record (FormFieldRow e i o))
     , Row.Cons name (InputField e i o) trash0 inputs
     , Row.Cons name (FormField e i o) trash1 row
     , Row1Cons name (FormField e i o) from to
     , ReplaceFormFieldInputs inputs tail row from
     )
  => ReplaceFormFieldInputs inputs (RL.Cons name (FormField e i o) tail) row to where
  replaceFormFieldInputsBuilder ir _ fr =
    first <<< rest
    where
      _name = SProxy :: SProxy name

      i :: InputField e i o
      i = Record.get _name ir

      f = unwrap $ Record.get _name fr
      first = Builder.insert _name (FormField $ f { input = unwrap i, touched = false, result = Nothing })
      rest = replaceFormFieldInputsBuilder ir (RLProxy :: RLProxy tail) fr

