module Formless.Transform.Record where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Formless.Internal (class Row1Cons)
import Formless.Spec (FormField(..), InputField(..), OutputField(..), U)
import Formless.Validation (Validation, runValidation)
import Heterogeneous.Folding as HF
import Heterogeneous.Mapping as HM
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Unsafe (unsafeGet, unsafeSet)
import Unsafe.Coerce (unsafeCoerce)

----------
-- Don't Tell Your Boss

-- | Given a variant of InputField and a record with the same labels but
-- | FormField values, replace the input of the form field.
unsafeSetInputVariant
  :: ∀ form x y
   . Newtype (form Variant InputField) (Variant x)
  => Newtype (form Record FormField) { | y }
  => form Variant InputField
  -> form Record FormField
  -> form Record FormField
unsafeSetInputVariant var rec = wrap $ unsafeSet (fst rep) val (unwrap rec)
  where
    rep :: ∀ e i o. Tuple String (InputField e i o)
    rep = case unsafeCoerce (unwrap var) of
      VariantRep x -> Tuple x.type x.value

    val :: ∀ e i o. FormField e i o
    val = case unsafeGet (fst rep) (unwrap rec) of
      FormField x -> FormField $ x { input = unwrap (snd rep) }

unsafeRunValidationVariant
  :: ∀ form x y z m
   . Monad m
  => Newtype (form Variant U) (Variant x)
  => Newtype (form Record FormField) { | y }
  => Newtype (form Record (Validation form m)) { | z }
  => form Variant U
  -> form Record (Validation form m)
  -> form Record FormField
  -> m (form Record FormField)
unsafeRunValidationVariant var vs rec = rec2
  where
    label :: String
    label = case unsafeCoerce (unwrap var) of
      VariantRep x -> x.type

    rec2 :: m (form Record FormField)
    rec2 = case unsafeGet label (unwrap rec) of
      FormField x -> do
        res <- runValidation (unsafeGet label $ unwrap vs) rec x.input
        let rec' = unsafeSet label (FormField $ x { result = Just res }) (unwrap rec)
        pure (wrap rec')


----------
-- Zips

-- | Apply a record of validators to form fields to produce a new set of
-- | validated form fields
data ApplyV form fns = ApplyV (form Record FormField) { | fns }

instance applyValidation'
  :: ( IsSymbol sym
     , Row.Cons sym (Validation form m e i o) x fns
     , Monad m
     , Newtype (form Record FormField) { | r1 }
     )
  => HM.MappingWithIndex (ApplyV form fns) (SProxy sym) (FormField e i o) (m (FormField e i o)) where
  mappingWithIndex (ApplyV form fns) prop (FormField field@{ input }) = do
    let validator = Record.get prop fns
    res <- runValidation validator form input
    pure $ FormField $ field { result = Just res }

-- Will produce a form full of monadic values that need to be sequenced
applyValidation
  :: ∀ form fns r0 r1
   . HM.HMapWithIndex (ApplyV form fns) r0 r1
  => form Record FormField
  -> { | fns }
  -> r0
  -> r1
applyValidation form = HM.hmapWithIndex <<< ApplyV form


-- | Replace a set of form field inputs with new inputs provided as `InputField`s
newtype ReplaceInput r = ReplaceInput { | r }

instance replaceInput
  :: ( IsSymbol sym
     , Row.Cons sym (InputField e i o) t0 r
     )
  => HM.MappingWithIndex (ReplaceInput r) (SProxy sym) (FormField e i o) (FormField e i o) where
  mappingWithIndex (ReplaceInput r) prop =
    let input = unwrap $ Record.get prop r
     in over FormField (_ { input = input })

replaceFormFieldInputs :: ∀ r0 r1 r2. HM.HMapWithIndex (ReplaceInput r0) r1 r2 => { | r0 } -> r1 -> r2
replaceFormFieldInputs = HM.hmapWithIndex <<< ReplaceInput


----------
-- Folds

-- | Verify every 'touched' field in the form is 'true'
data Touched = Touched

instance touched' :: HF.Folding Touched Boolean (FormField e i o) Boolean where
  folding Touched acc (FormField { touched }) = acc && touched

allTouched :: ∀ r0. HF.HFoldl Touched Boolean r0 Boolean => r0 -> Boolean
allTouched = HF.hfoldl Touched true


-- | Count the total errors in the form
data CountError = CountError

instance countError' :: HF.Folding CountError Int (FormField e i o) Int where
  folding CountError acc (FormField { result }) =
    acc + case result of
      Just (Left _) -> 1
      _ -> 0

countErrors :: ∀ r0. HF.HFoldl CountError Int r0 Int => r0 -> Int
countErrors = HF.hfoldl CountError 0


----------
-- Maps

-- | Transform a record of form fields into a record of input fields
data FormFieldToInputField = FormFieldToInputField

instance formFieldToInputField
  :: HM.Mapping FormFieldToInputField (FormField e i o) (InputField e i o) where
  mapping FormFieldToInputField (FormField { input }) = InputField input

formFieldsToInputFields :: ∀ r0 r1. HM.HMap FormFieldToInputField r0 r1 => r0 -> r1
formFieldsToInputFields = HM.hmap FormFieldToInputField


-- | Transform a record of input fields into a record of form fields
data InputFieldToFormField = InputFieldToFormField

instance inputFieldToFormField
  :: HM.Mapping InputFieldToFormField (InputField e i o) (FormField e i o) where
  mapping InputFieldToFormField (InputField input) =
    FormField { input, touched: false, result: Nothing }

inputFieldsToFormFields :: ∀ r0 r1. HM.HMap InputFieldToFormField r0 r1 => r0 -> r1
inputFieldsToFormFields = HM.hmap InputFieldToFormField


-- | Set all form fields 'touched' field
data SetFormFieldTouched = SetFormFieldTouched Boolean

instance setFormFieldTouched
  :: HM.Mapping SetFormFieldTouched (FormField e i o) (FormField e i o) where
  mapping (SetFormFieldTouched bool) = over FormField (_ { touched = bool })

setFormFieldsTouched :: ∀ r0 r1. HM.HMap SetFormFieldTouched r0 r1 => Boolean -> r0 -> r1
setFormFieldsTouched = HM.hmap <<< SetFormFieldTouched


-- | Unwrap every newtype in a record filled with newtypes
data UnwrapField = UnwrapField

instance unwrapField :: (Newtype wrapper x) => HM.Mapping UnwrapField wrapper x where
  mapping UnwrapField = unwrap

unwrapRecord :: ∀ r0 r1. HM.HMap UnwrapField r0 r1 => r0 -> r1
unwrapRecord = HM.hmap UnwrapField


-- | Wrap every field in a record with a particular newtype
data WrapField = WrapField

instance wrapField :: (Newtype wrapper x) => HM.Mapping WrapField x wrapper where
  mapping WrapField = wrap

wrapRecord :: ∀ r0 r1. HM.HMap WrapField r0 r1 => r0 -> r1
wrapRecord = HM.hmap WrapField


-- | Attempt to retrieve an OutputField for every result
data MaybeOutput = MaybeOutput

instance maybeOutput :: HM.Mapping MaybeOutput (FormField e i o) (Maybe (OutputField e i o)) where
  mapping MaybeOutput (FormField { result }) = map OutputField =<< hush <$> result

-- | For internal use. Can be used in conjunction with sequenceRecord to produce a Maybe record
-- | of output fields.
formFieldsToMaybeOutputFields :: ∀ r0 r1. HM.HMap MaybeOutput r0 r1 => r0 -> r1
formFieldsToMaybeOutputFields = HM.hmap MaybeOutput


----------
-- Helpers

-- @natefaubion
-- | Sequence a record of `f values` into an `f` record of values
data FoldSequenceMember (f :: Type -> Type) = FoldSequenceMember

instance foldSequenceMember1
  :: (Applicative f, Row1Cons sym a r2 r3, IsSymbol sym)
  => HF.FoldingWithIndex
       (FoldSequenceMember f)
       (SProxy sym)
       (f (Builder { | r1 } { | r2 }))
       (f a)
       (f (Builder { | r1 } { | r3 })) where
  foldingWithIndex FoldSequenceMember prop b1 fa =
    (>>>) <$> b1 <*> (Builder.insert prop <$> fa)

else instance foldSequenceMember2
  :: (Functor f, Row1Cons sym a r2 r3, IsSymbol sym)
  => HF.FoldingWithIndex
       (FoldSequenceMember f)
       (SProxy sym)
       (f (Builder { | r1 } { | r2 }))
       a
       (f (Builder { | r1 } { | r3 })) where
  foldingWithIndex FoldSequenceMember prop b1 a = (_ >>> Builder.insert prop a) <$> b1

sequenceRecord
  :: ∀ f r1 r2
   . Applicative f
  => HF.HFoldlWithIndex (FoldSequenceMember f) (f (Builder {} {})) { | r1 } (f (Builder {} { | r2 }))
  => { | r1 }
  -> f { | r2 }
sequenceRecord =
  map (flip Builder.build {}) <$>
    HF.hfoldlWithIndex FoldSequenceMember (pure identity :: f (Builder {} {}))
