module Formless.Transform.Record where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Formless.Spec (FormField(..), InputField(..), OutputField(..))
import Heterogeneous.Folding as HF
import Heterogeneous.Mapping as HM
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder

----------
-- Scratch

test :: forall e0 e1 o0 o1.
   { x1 :: FormField e0 String o0
   , x2 :: FormField e1 Int o1
   }
test =
  { x1: FormField { input: "hello", touched: true, result: Nothing }
  , x2: FormField { input: 1, touched: false, result: Nothing }
  }

testI :: forall t18 t20 t21 t23.
   { x1 :: InputField t20 String t18
   , x2 :: InputField t23 Int t21
   }
testI =
  { x1: InputField ""
  , x2: InputField 0
  }

----------
-- Zips

-- TODO:
-- applyValidation

newtype ReplaceInput r = ReplaceInput { | r }

instance replaceInput
  :: ( IsSymbol sym
     , Row.Cons sym (InputField e i o) t0 r
     )
  => HM.MappingWithIndex (ReplaceInput r) (SProxy sym) (FormField e i o) (FormField e i o) where
  mappingWithIndex (ReplaceInput r) prop =
    let input = unwrap $ Record.get prop r
     in over FormField (_ { input = input })

replaceFormFieldInputs
  :: ∀ r0 r1 r2
   . HM.HMapWithIndex (ReplaceInput r0) r1 r2
  => { | r0 }
  -> r1
  -> r2
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

-- TODO:
-- setInputV
-- setValidationV

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
data FoldSequenceMember (f :: Type -> Type) = FoldSequenceMember

instance foldSequenceMember1
  :: ( Applicative f
     , Row.Cons sym a r2 r3
     , Row.Lacks sym r2
     , IsSymbol sym
     )
  => HF.FoldingWithIndex
       (FoldSequenceMember f)
       (SProxy sym)
       (f (Builder { | r1 } { | r2 }))
       (f a)
       (f (Builder { | r1 } { | r3 })) where
  foldingWithIndex FoldSequenceMember prop b1 fa =
    (>>>) <$> b1 <*> (Builder.insert prop <$> fa)

else instance foldSequenceMember2
  :: ( Functor f
     , Row.Cons sym a r2 r3
     , Row.Lacks sym r2
     , IsSymbol sym
     )
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
  => HF.HFoldlWithIndex
       (FoldSequenceMember f)
       (f (Builder {} {}))
       { | r1 }
       (f (Builder {} { | r2 }))
  => { | r1 }
  -> f { | r2 }
sequenceRecord =
  map (flip Builder.build {}) <$>
    HF.hfoldlWithIndex FoldSequenceMember (pure identity :: f (Builder {} {}))
