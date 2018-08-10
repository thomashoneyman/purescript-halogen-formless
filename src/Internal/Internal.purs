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
  :: ∀ form fields xs m
   . RL.RowToList fields xs
  => AllTouched xs fields
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record (FormField m)
  -> Boolean
checkTouched = allTouchedImpl (RLProxy :: RLProxy xs) <<< unwrap

-- | A helper function that will count all errors in a record
countErrors
  :: ∀ form fs xs row fields m
   . RL.RowToList row xs
  => RL.RowToList fields fs
  => CountErrors fs fields row
  => SumRecord xs row (Additive Int)
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record (FormField m)
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
  :: ∀ xs form fields inputs m
   . RL.RowToList fields xs
  => FormFieldsToInputFields xs fields inputs
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record (FormField m)
  -> form Record InputField
formFieldsToInputFields r = wrap $ fromScratch builder
  where builder = formFieldsToInputFieldsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of FormField(s).
inputFieldsToFormFields
  :: ∀ xs form inputs fields m
   . RL.RowToList inputs xs
  => InputFieldsToFormFields xs inputs fields
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record InputField
  -> form Record (FormField m)
inputFieldsToFormFields r = wrap $ fromScratch builder
  where builder = inputFieldsToFormFieldsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | An intermediate function that transforms a record of FormField into a record
inputFieldToMaybeOutput
  :: ∀ xs form fields outputs m
   . RL.RowToList fields xs
  => Newtype (form Record (FormField m)) (Record fields)
  => Newtype (form Record OutputField) (Record outputs)
  => FormFieldToMaybeOutput xs fields outputs
  => form Record (FormField m)
  -> Maybe (form Record OutputField)
inputFieldToMaybeOutput r = map wrap $ fromScratch <$> builder
  where builder = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy xs) (unwrap r)

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
     , Row.Cons name (FormField m e i o) trash row
     , FormFieldsToInputFields tail row from
     , Row1Cons name (InputField e i o) from to
     )
  => FormFieldsToInputFields (RL.Cons name (FormField m e i o) tail) row to where
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
     , Row1Cons name (FormField m e i o) from to
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
        , validator: Nothing
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
     , Row.Cons name (FormField m e i o) trash row
     , FormFieldToMaybeOutput tail row from
     , Row1Cons name (OutputField e i o) from to
     )
  => FormFieldToMaybeOutput (RL.Cons name (FormField m e i o) tail) row to where
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
     , Row.Cons name (FormField m e i o) trash row
     , CountErrors tail row from
     , Row1Cons name (Additive Int) from to
     )
  => CountErrors (RL.Cons name (FormField m e i o) tail) row to where
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
     , Row.Cons name (FormField m e i o) t0 r
     , AllTouched tail r
     )
  => AllTouched (RL.Cons name (FormField m e i o) tail) r
  where
    allTouchedImpl _ r =
      if (unwrap $ Record.get (SProxy :: SProxy name) r).touched
        then allTouchedImpl (RLProxy :: RLProxy tail) r
        else false

-- | @monoidmusician
class UpdateInputVariant r to m where
  updateInputVariant
    :: Monad m
    => (∀ e i o. InputField e i o -> FormField m e i o -> m (FormField m e i o))
    -> Variant r
    -> Record to
    -> m (Record to)

instance updateInputVariant' ::
  ( RL.RowToList r rl
  , UpdateInputVariantRL rl r to m
  ) => UpdateInputVariant r to m where
    updateInputVariant f = updateInputVariantRL f (RLProxy :: RLProxy rl)

class UpdateInputVariantRL rl v to m | rl -> v where
  updateInputVariantRL
    :: Monad m
    => (∀ e i o. InputField e i o -> FormField m e i o -> m (FormField m e i o))
    -> RLProxy rl
    -> Variant v
    -> Record to
    -> m (Record to)

instance updateInputVariantNil :: UpdateInputVariantRL RL.Nil () to m where
  updateInputVariantRL _ _ = case_

instance updateInputVariantCons ::
  ( IsSymbol s
  , UpdateInputVariantRL rl v to m
  , Row.Cons s (FormField m e i o) t0 to
  , Row.Cons s (InputField e i o) v v'
  ) => UpdateInputVariantRL (RL.Cons s (InputField e i o) rl) v' to m where
    updateInputVariantRL f _ =
      on s f' (updateInputVariantRL f (RLProxy :: RLProxy rl))
      where
        f' :: InputField e i o -> { | to } -> m ({ | to })
        f' a r = do
          let x = Record.get s r
          res <- f a x
          pure $ Record.set s res r

        s = SProxy :: SProxy s


----------
-- Transform form fields
----------

-- | Transform form fields, with pure results
transformFormFields
  :: ∀ xs form m fields
   . RL.RowToList fields xs
  => TransformFormFields xs fields fields m
  => Newtype (form Record (FormField m)) (Record fields)
  => (∀ e i o. FormField m e i o -> FormField m e i o)
  -> form Record (FormField m)
  -> form Record (FormField m)
transformFormFields f r = wrap $ fromScratch builder
  where builder = transformFormFieldsBuilder f (RLProxy :: RLProxy xs) (unwrap r)

class TransformFormFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) m | xs -> to where
  transformFormFieldsBuilder :: (∀ e i o. FormField m e i o -> FormField m e i o) -> RLProxy xs -> Record row -> FromScratch to

instance transformFormFieldsTouchedNil :: TransformFormFields RL.Nil row () m where
  transformFormFieldsBuilder _ _ _ = identity

instance transformFormFieldsTouchedCons
  :: ( IsSymbol name
     , Row.Cons name (FormField m e i o) trash row
     , Row1Cons name (FormField m e i o) from to
     , TransformFormFields tail row from m
     )
  => TransformFormFields (RL.Cons name (FormField m e i o) tail) row to m where
  transformFormFieldsBuilder f _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      first = Builder.insert _name (f (Record.get _name r))
      rest = transformFormFieldsBuilder f (RLProxy :: RLProxy tail) r


-- | Transform form fields, with monadic results.
transformFormFieldsM
  :: ∀ xs form fields m
   . RL.RowToList fields xs
  => Monad m
  => TransformFormFieldsM xs fields fields m
  => Newtype (form Record (FormField m)) (Record fields)
  => (∀ e i o. FormField m e i o -> m (FormField m e i o))
  -> form Record (FormField m)
  -> m (form Record (FormField m))
transformFormFieldsM f r = map wrap $ fromScratch <$> builder
  where
    builder = transformFormFieldsBuilderM f (RLProxy :: RLProxy xs) (unwrap r)

class TransformFormFieldsM (xs :: RL.RowList) (row :: # Type) (to :: # Type) m | xs -> to where
  transformFormFieldsBuilderM :: (∀ e i o. FormField m e i o -> m (FormField m e i o)) -> RLProxy xs -> Record row -> m (FromScratch to)

instance transformFormFieldsTouchedNilM :: Monad m => TransformFormFieldsM RL.Nil row () m where
  transformFormFieldsBuilderM _ _ _ = pure identity

instance transformFormFieldsTouchedConsM
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (FormField m e i o) trash row
     , TransformFormFieldsM tail row from m
     , Row1Cons name (FormField m e i o) from to
     )
  => TransformFormFieldsM (RL.Cons name (FormField m e i o) tail) row to m where
  transformFormFieldsBuilderM f _ r =
    fn <$> val <*> rest
    where
      _name = SProxy :: SProxy name
      rest = transformFormFieldsBuilderM f (RLProxy :: RLProxy tail) r
      fn val' rest' = Builder.insert _name val' <<< rest'

      val :: m (FormField m e i o)
      val = do
        let (x :: FormField m e i o) = Record.get _name r
        f x

----------
-- Replace form fields
----------

replaceFormFieldInputs
  :: ∀ xs form m fields inputs
   . RL.RowToList fields xs
  => ReplaceFormFieldInputs inputs xs fields fields
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record InputField
  -> form Record (FormField m)
  -> form Record (FormField m)
replaceFormFieldInputs inputs fields = wrap $ fromScratch builder
  where builder = replaceFormFieldInputsBuilder (unwrap inputs) (RLProxy :: RLProxy xs) (unwrap fields)

class ReplaceFormFieldInputs (inputs :: # Type) (xs :: RL.RowList) (fields :: # Type) (to :: # Type) | xs -> to where
  replaceFormFieldInputsBuilder ::  Record inputs -> RLProxy xs -> Record fields -> FromScratch to

instance replaceFormFieldInputsTouchedNil :: ReplaceFormFieldInputs inputs RL.Nil fields () where
  replaceFormFieldInputsBuilder _ _ _ = identity

instance replaceFormFieldInputsTouchedCons
  :: ( IsSymbol name
     , Newtype (InputField e i o) i
     , Newtype (FormField m e i o) (Record (FormFieldRow m e i o))
     , Row.Cons name (InputField e i o) trash0 inputs
     , Row.Cons name (FormField m e i o) trash1 row
     , Row1Cons name (FormField m e i o) from to
     , ReplaceFormFieldInputs inputs tail row from
     )
  => ReplaceFormFieldInputs inputs (RL.Cons name (FormField m e i o) tail) row to where
  replaceFormFieldInputsBuilder ir _ fr =
    first <<< rest
    where
      _name = SProxy :: SProxy name

      i :: InputField e i o
      i = Record.get _name ir

      f = unwrap $ Record.get _name fr
      first = Builder.insert _name (FormField $ f { input = unwrap i, touched = false, result = Nothing })
      rest = replaceFormFieldInputsBuilder ir (RLProxy :: RLProxy tail) fr


replaceFormFieldValidators
  :: ∀ xs form m fields vs
   . RL.RowToList fields xs
  => ReplaceFormFieldValidators vs xs fields fields
  => Newtype (form Record (Validation m)) (Record vs)
  => Newtype (form Record (FormField m)) (Record fields)
  => form Record (Validation m)
  -> form Record (FormField m)
  -> form Record (FormField m)
replaceFormFieldValidators vs fields = wrap $ fromScratch builder
  where builder = replaceFormFieldValidatorsBuilder (unwrap vs) (RLProxy :: RLProxy xs) (unwrap fields)

class ReplaceFormFieldValidators (vs :: # Type) (xs :: RL.RowList) (fields :: # Type) (to :: # Type) | xs -> to where
  replaceFormFieldValidatorsBuilder ::  Record vs -> RLProxy xs -> Record fields -> FromScratch to

instance replaceFormFieldValidatorsTouchedNil :: ReplaceFormFieldValidators vs RL.Nil fields () where
  replaceFormFieldValidatorsBuilder _ _ _ = identity

instance replaceFormFieldValidatorsTouchedCons
  :: ( IsSymbol name
     , Newtype (Validation m e i o) (i -> m (Either e o))
     , Newtype (FormField m e i o) (Record (FormFieldRow m e i o))
     , Row.Cons name (Validation m e i o) trash0 vs
     , Row.Cons name (FormField m e i o) trash1 row
     , Row1Cons name (FormField m e i o) from to
     , ReplaceFormFieldValidators vs tail row from
     )
  => ReplaceFormFieldValidators vs (RL.Cons name (FormField m e i o) tail) row to where
  replaceFormFieldValidatorsBuilder vr _ fr =
    first <<< rest
    where
      _name = SProxy :: SProxy name

      v :: Validation m e i o
      v = Record.get _name vr

      f = unwrap $ Record.get _name fr
      first = Builder.insert _name (FormField $ f { result = Nothing, touched = false, validator = Just (unwrap v) })
      rest = replaceFormFieldValidatorsBuilder vr (RLProxy :: RLProxy tail) fr

