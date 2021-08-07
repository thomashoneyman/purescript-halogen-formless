module Formless.Internal.Transform where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Formless.Data.FormFieldResult (FormFieldResult(..), fromEither, toMaybe)
import Formless.Types.Form (FormField(..), FormFieldRow, InputField(..), InputFunction, OutputField(..), U)
import Formless.Validation (Validation, runValidation)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

----------
-- Utilities

-- | Apply a builder that produces an output record from an empty record
fromScratch :: forall r. FromScratch r -> Record r
fromScratch = Builder.build <@> {}

-- | Represents building some output record from an empty record
type FromScratch r = Builder {} (Record r)

-- | A constraint synonym for Row.Cons and Row.Lacks
class Row1Cons :: forall k. Symbol -> k -> Row k -> Row k -> Constraint
class (Row.Cons s t r r', Row.Lacks s r) <= Row1Cons s t r r' | s t r -> r', s r' -> t r
instance row1Cons :: (Row.Cons s t r r', Row.Lacks s r) => Row1Cons s t r r'

-----
-- Functions

-- | A helper function that will count all errors in a record
allTouched
  :: forall form fs fxs
   . RL.RowToList fs fxs
  => AllTouched fxs fs
  => Newtype (form Record FormField) { | fs }
  => form Record FormField
  -> Boolean
allTouched = allTouchedImpl (Proxy :: Proxy fxs) <<< unwrap

-- | A helper function that will count all errors in a record
countErrors
  :: forall form fs fxs
   . RL.RowToList fs fxs
  => CountErrors fxs fs
  => Newtype (form Record FormField) { | fs }
  => form Record FormField
  -> Int
countErrors = countErrorsImpl (Proxy :: Proxy fxs) <<< unwrap

-- | A helper function that will automatically transform a record of FormField(s) into
-- | just the input value
setFormFieldsTouched
  :: forall fxs form fs
   . RL.RowToList fs fxs
  => SetFormFieldsTouched fxs fs fs
  => Newtype (form Record FormField) { | fs }
  => form Record FormField
  -> form Record FormField
setFormFieldsTouched r = wrap $ fromScratch builder
  where
  builder = setFormFieldsTouchedBuilder (Proxy :: Proxy fxs) (unwrap r)

-- | A helper function that will automatically transform a record of FormField(s) into
-- | just the input value
formFieldsToInputFields
  :: forall fxs form fs is
   . RL.RowToList fs fxs
  => FormFieldsToInputFields fxs fs is
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record FormField) { | fs }
  => form Record FormField
  -> form Record InputField
formFieldsToInputFields r = wrap $ fromScratch builder
  where
  builder = formFieldsToInputFieldsBuilder (Proxy :: Proxy fxs) (unwrap r)

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of FormField(s).
inputFieldsToFormFields
  :: forall ixs form is fs
   . RL.RowToList is ixs
  => InputFieldsToFormFields ixs is fs
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record FormField) { | fs }
  => form Record InputField
  -> form Record FormField
inputFieldsToFormFields r = wrap $ fromScratch builder
  where
  builder = inputFieldsToFormFieldsBuilder (Proxy :: Proxy ixs) (unwrap r)

-- | An intermediate function that transforms a record of FormField into a record
formFieldsToMaybeOutputFields
  :: forall fxs form fs os
   . RL.RowToList fs fxs
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => FormFieldToMaybeOutput fxs fs os
  => form Record FormField
  -> Maybe (form Record OutputField)
formFieldsToMaybeOutputFields r = map wrap $ fromScratch <$> builder
  where
  builder = formFieldsToMaybeOutputBuilder (Proxy :: Proxy fxs) (unwrap r)

replaceFormFieldInputs
  :: forall fxs form fs is
   . RL.RowToList fs fxs
  => ReplaceFormFieldInputs is fxs fs fs
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record FormField) { | fs }
  => form Record InputField
  -> form Record FormField
  -> form Record FormField
replaceFormFieldInputs is fs = wrap $ fromScratch builder
  where
  builder = replaceFormFieldInputsBuilder (unwrap is) (Proxy :: Proxy fxs) (unwrap fs)

modifyAll
  :: forall fxs form fs ifs
   . RL.RowToList fs fxs
  => ModifyAll ifs fxs fs fs
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => form Record InputFunction
  -> form Record FormField
  -> form Record FormField
modifyAll ifs fs = wrap $ fromScratch builder
  where
  builder = modifyAllBuilder (unwrap ifs) (Proxy :: Proxy fxs) (unwrap fs)

validateAll
  :: forall vs fxs form fs m
   . RL.RowToList fs fxs
  => Monad m
  => ValidateAll vs fxs fs fs m
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Record FormField) { | fs }
  => form Record (Validation form m)
  -> form Record FormField
  -> m (form Record FormField)
validateAll vs fs = map wrap $ fromScratch <$> builder
  where
  builder = validateAllBuilder (unwrap vs) (Proxy :: Proxy fxs) (unwrap fs)

----------
-- Don't Tell Your Boss

-- | Given a variant of InputFunction and a record with the same labels but
-- | FormField values, replace the input of the form field. In addition, modify
-- | the form field result to represent whether async validation is going to
-- | occur.
unsafeModifyInputVariant
  :: forall form x y
   . Newtype (form Variant InputFunction) (Variant x)
  => Newtype (form Record FormField) { | y }
  => (forall e o. FormFieldResult e o -> FormFieldResult e o)
  -> form Variant InputFunction
  -> form Record FormField
  -> form Record FormField
unsafeModifyInputVariant f var rec = wrap $ unsafeSet (fst rep) val (unwrap rec)
  where
  rep :: forall e i o. Tuple String (InputFunction e i o)
  rep = case unsafeCoerce (unwrap var) of
    VariantRep x -> Tuple x.type x.value

  val :: forall e i o. FormField e i o
  val = case unsafeGet (fst rep) (unwrap rec) of
    FormField x -> FormField $ x
      { input = unwrap (snd rep) $ x.input
      , touched = true
      , result = f x.result
      }

unsafeRunValidationVariant
  :: forall form x y z m
   . Monad m
  => Newtype (form Variant U) (Variant x)
  => Newtype (form Record FormField) { | y }
  => Newtype (form Record (Validation form m)) { | z }
  => form Variant U
  -> form Record (Validation form m)
  -> form Record FormField
  -> m ((form Record FormField) -> (form Record FormField))
unsafeRunValidationVariant var vs rec = rec2
  where
  label :: String
  label = case unsafeCoerce (unwrap var) of
    VariantRep x -> x.type

  rec2 :: m ((form Record FormField) -> (form Record FormField))
  rec2 = case unsafeGet label (unwrap rec) of
    FormField x -> do
      res <- runValidation (unsafeGet label $ unwrap vs) rec x.input
      pure (\newRec -> wrap $ unsafeSet label (FormField $ x { result = fromEither res }) (unwrap newRec))

-----
-- Classes (Internal)

-- | The class that provides the Builder implementation to set all form fields touched
class SetFormFieldsTouched (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) | xs -> to where
  setFormFieldsTouchedBuilder :: Proxy xs -> Record row -> FromScratch to

instance setFormFieldsTouchedNil :: SetFormFieldsTouched RL.Nil row () where
  setFormFieldsTouchedBuilder _ _ = identity

instance setFormFieldsTouchedCons ::
  ( IsSymbol name
  , Row.Cons name (FormField e i o) t0 row
  , SetFormFieldsTouched tail row from
  , Row1Cons name (FormField e i o) from to
  ) =>
  SetFormFieldsTouched (RL.Cons name (FormField e i o) tail) row to where
  setFormFieldsTouchedBuilder _ r =
    first <<< rest
    where
    _name = Proxy :: Proxy name
    val = over FormField (_ { touched = true }) $ Record.get _name r
    rest = setFormFieldsTouchedBuilder (Proxy :: Proxy tail) r
    first = Builder.insert _name val

----------
-- Transform form fields to inputs

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormField to record of InputField.
class FormFieldsToInputFields (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) | xs -> to where
  formFieldsToInputFieldsBuilder :: Proxy xs -> Record row -> FromScratch to

instance inputFieldsToInputNil :: FormFieldsToInputFields RL.Nil row () where
  formFieldsToInputFieldsBuilder _ _ = identity

instance inputFieldsToInputCons ::
  ( IsSymbol name
  , Row.Cons name (FormField e i o) trash row
  , FormFieldsToInputFields tail row from
  , Row1Cons name (InputField e i o) from to
  ) =>
  FormFieldsToInputFields (RL.Cons name (FormField e i o) tail) row to where
  formFieldsToInputFieldsBuilder _ r =
    first <<< rest
    where
    _name = Proxy :: Proxy name
    val = transform $ Record.get _name r
    rest = formFieldsToInputFieldsBuilder (Proxy :: Proxy tail) r
    first = Builder.insert _name val
    transform (FormField fs) = InputField fs.input

----------
-- Transform input fields to form fields

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of InputField to record of FormField.
class InputFieldsToFormFields (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) | xs -> to where
  inputFieldsToFormFieldsBuilder :: Proxy xs -> Record row -> FromScratch to

instance inputFieldsToFormFieldsNil :: InputFieldsToFormFields RL.Nil row () where
  inputFieldsToFormFieldsBuilder _ _ = identity

instance inputFieldsToFormFieldsCons ::
  ( IsSymbol name
  , Row.Cons name (InputField e i o) trash row
  , InputFieldsToFormFields tail row from
  , Row1Cons name (FormField e i o) from to
  ) =>
  InputFieldsToFormFields (RL.Cons name (InputField e i o) tail) row to where
  inputFieldsToFormFieldsBuilder _ r =
    first <<< rest
    where
    _name = Proxy :: Proxy name
    val = transform $ Record.get _name r
    rest = inputFieldsToFormFieldsBuilder (Proxy :: Proxy tail) r
    first = Builder.insert _name val
    transform (InputField input) = FormField { input, touched: false, result: NotValidated }

----------
-- Flip all form fields if valid

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of MaybeOutput to a record of OutputField, but only if all fs were successfully
-- | validated.
class FormFieldToMaybeOutput (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) | xs -> to where
  formFieldsToMaybeOutputBuilder :: Proxy xs -> Record row -> Maybe (FromScratch to)

instance formFieldsToMaybeOutputNil :: FormFieldToMaybeOutput RL.Nil row () where
  formFieldsToMaybeOutputBuilder _ _ = Just identity

instance formFieldsToMaybeOutputCons ::
  ( IsSymbol name
  , Row.Cons name (FormField e i o) trash row
  , FormFieldToMaybeOutput tail row from
  , Row1Cons name (OutputField e i o) from to
  ) =>
  FormFieldToMaybeOutput (RL.Cons name (FormField e i o) tail) row to where
  formFieldsToMaybeOutputBuilder _ r =
    transform <$> val <*> rest
    where
    _name = Proxy :: Proxy name

    val :: Maybe (OutputField e i o)
    val = OutputField <$> toMaybe (unwrap $ Record.get _name r).result

    rest :: Maybe (FromScratch from)
    rest = formFieldsToMaybeOutputBuilder (Proxy :: Proxy tail) r

    transform :: OutputField e i o -> FromScratch from -> FromScratch to
    transform v builder' = Builder.insert _name v <<< builder'

-- | A class to check if all fs in an FormField record have been touched or not
class CountErrors (rl :: RL.RowList Type) (r :: Row Type) where
  countErrorsImpl :: Proxy rl -> Record r -> Int

instance nilCountErrors :: CountErrors RL.Nil r where
  countErrorsImpl _ _ = 0

instance consCountErrors ::
  ( IsSymbol name
  , Row.Cons name (FormField e i o) t0 r
  , CountErrors tail r
  ) =>
  CountErrors (RL.Cons name (FormField e i o) tail) r where
  countErrorsImpl _ r = do
    let
      res = case (unwrap $ Record.get (Proxy :: Proxy name) r).result of
        Error _ -> 1
        _ -> 0
    res + countErrorsImpl (Proxy :: Proxy tail) r

----------
-- Check if all form fields are touched

-- | A class to check if all fs in an FormField record have been touched or not
class AllTouched (rl :: RL.RowList Type) (r :: Row Type) where
  allTouchedImpl :: Proxy rl -> Record r -> Boolean

instance nilAllTouched :: AllTouched RL.Nil r where
  allTouchedImpl _ _ = true

instance consAllTouched ::
  ( IsSymbol name
  , Row.Cons name (FormField e i o) t0 r
  , AllTouched tail r
  ) =>
  AllTouched (RL.Cons name (FormField e i o) tail) r where
  allTouchedImpl _ r =
    if (unwrap $ Record.get (Proxy :: Proxy name) r).touched then allTouchedImpl (Proxy :: Proxy tail) r
    else false

----------
-- Apply form field validation

-- | A class that applies the current state to the unwrapped version of every validator
class ValidateAll (vs :: Row Type) (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) m | xs -> to where
  validateAllBuilder :: Record vs -> Proxy xs -> Record row -> m (FromScratch to)

instance applyToValidationNil :: Monad m => ValidateAll vs RL.Nil row () m where
  validateAllBuilder _ _ _ = pure identity

instance applyToValidationCons ::
  ( IsSymbol name
  , Monad m
  , Row.Cons name (FormField e i o) t0 row
  , Newtype (form Record FormField) { | row }
  , Row.Cons name (Validation form m e i o) t1 vs
  , Row1Cons name (FormField e i o) from to
  , ValidateAll vs tail row from m
  ) =>
  ValidateAll vs (RL.Cons name (FormField e i o) tail) row to m where
  validateAllBuilder vs _ r =
    fn <$> val <*> rest
    where
    _name = Proxy :: Proxy name
    fn val' rest' = Builder.insert _name val' <<< rest'
    rest = validateAllBuilder vs (Proxy :: Proxy tail) r
    val = do
      let
        validator = unwrap $ Record.get _name vs
        formField = unwrap $ Record.get _name r
      res <- validator (wrap r) formField.input
      pure $ wrap $ formField { result = fromEither res }

--------
-- Apply modifications across a record

class ModifyAll (ifs :: Row Type) (xs :: RL.RowList Type) (fs :: Row Type) (to :: Row Type) | xs -> to where
  modifyAllBuilder :: Record ifs -> Proxy xs -> Record fs -> FromScratch to

instance modifyAllNil :: ModifyAll ifs RL.Nil fs () where
  modifyAllBuilder _ _ _ = identity

instance modifyAllCons ::
  ( IsSymbol name
  , Newtype (InputFunction e i o) (i -> i)
  , Newtype (FormField e i o) { | (FormFieldRow e i o) }
  , Row.Cons name (InputFunction e i o) trash0 ifs
  , Row.Cons name (FormField e i o) trash1 row
  , Row1Cons name (FormField e i o) from to
  , ModifyAll ifs tail row from
  ) =>
  ModifyAll ifs (RL.Cons name (FormField e i o) tail) row to where
  modifyAllBuilder ifs _ r = first <<< rest
    where
    _name = Proxy :: Proxy name
    f = unwrap $ Record.get _name ifs
    field = Record.get _name r
    rest = modifyAllBuilder ifs (Proxy :: Proxy tail) r
    first = Builder.insert _name (over FormField (\x -> x { input = f x.input }) field)

----------
-- Replace all form field inputs

class ReplaceFormFieldInputs (is :: Row Type) (xs :: RL.RowList Type) (fs :: Row Type) (to :: Row Type) | xs -> to where
  replaceFormFieldInputsBuilder :: Record is -> Proxy xs -> Record fs -> FromScratch to

instance replaceFormFieldInputsTouchedNil :: ReplaceFormFieldInputs is RL.Nil fs () where
  replaceFormFieldInputsBuilder _ _ _ = identity

instance replaceFormFieldInputsTouchedCons ::
  ( IsSymbol name
  , Newtype (InputField e i o) i
  , Newtype (FormField e i o) { | (FormFieldRow e i o) }
  , Row.Cons name (InputField e i o) trash0 is
  , Row.Cons name (FormField e i o) trash1 row
  , Row1Cons name (FormField e i o) from to
  , ReplaceFormFieldInputs is tail row from
  ) =>
  ReplaceFormFieldInputs is (RL.Cons name (FormField e i o) tail) row to where
  replaceFormFieldInputsBuilder ir _ fr = first <<< rest
    where
    _name = Proxy :: Proxy name
    i = Record.get _name ir
    f = unwrap $ Record.get _name fr
    rest = replaceFormFieldInputsBuilder ir (Proxy :: Proxy tail) fr
    first =
      Builder.insert
        _name
        (FormField $ f { input = unwrap i, touched = false, result = NotValidated })
