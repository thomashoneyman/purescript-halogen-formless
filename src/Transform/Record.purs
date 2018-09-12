module Formless.Transform.Record where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Formless.Spec (FormField(..), InputField(..))
import Formless.Transform.Types (FromScratch)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Record.Builder (build) as Builder

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

instance replaceInput'
  :: ( IsSymbol sym
     , Row.Cons sym (InputField e i o) t0 r
     )
  => MappingWithIndex (ReplaceInput r) (SProxy sym) (FormField e i o) (FormField e i o) where
  mappingWithIndex (ReplaceInput r) prop =
    let input = unwrap $ Record.get prop r
     in over FormField (_ { input = input })

replaceFormFieldInputs
  :: ∀ r0 r1 r2
   . HMapWithIndex (ReplaceInput r0) r1 r2
  => { | r0 }
  -> r1
  -> r2
replaceFormFieldInputs = hmapWithIndex <<< ReplaceInput


----------
-- Folds

-- | Verify every 'touched' field in the form is 'true'
data Touched = Touched

instance touched' :: Folding Touched Boolean (FormField e i o) Boolean where
  folding Touched acc (FormField { touched }) = acc && touched

allTouched :: ∀ r0. HFoldl Touched Boolean r0 Boolean => r0 -> Boolean
allTouched = hfoldl Touched true


-- | Count the total errors in the form
data CountError = CountError

instance countError' :: Folding CountError Int (FormField e i o) Int where
  folding CountError acc (FormField { result }) =
    acc + case result of
      Just (Left _) -> 1
      _ -> 0

countErrors :: ∀ r0. HFoldl CountError Int r0 Int => r0 -> Int
countErrors = hfoldl CountError 0


----------
-- Maps

-- TODO:
-- setInputV
-- setValidationV

-- | Transform a record of form fields into a record of input fields
data FormFieldToInputField = FormFieldToInputField

instance formFieldToInputField
  :: Mapping FormFieldToInputField (FormField e i o) (InputField e i o) where
  mapping FormFieldToInputField (FormField { input }) = InputField input

formFieldsToInputFields :: ∀ r0 r1. HMap FormFieldToInputField r0 r1 => r0 -> r1
formFieldsToInputFields = hmap FormFieldToInputField


-- | Transform a record of input fields into a record of form fields
data InputFieldToFormField = InputFieldToFormField

instance inputFieldToFormField
  :: Mapping InputFieldToFormField (InputField e i o) (FormField e i o) where
  mapping InputFieldToFormField (InputField input) =
    FormField { input, touched: false, result: Nothing }

inputFieldsToFormFields :: ∀ r0 r1. HMap InputFieldToFormField r0 r1 => r0 -> r1
inputFieldsToFormFields = hmap InputFieldToFormField


-- | Set all form fields 'touched' field
data SetFormFieldTouched = SetFormFieldTouched Boolean

instance setFormFieldTouched
  :: Mapping SetFormFieldTouched (FormField e i o) (FormField e i o) where
  mapping (SetFormFieldTouched bool) = over FormField (_ { touched = bool })

setFormFieldsTouched :: ∀ r0 r1. HMap SetFormFieldTouched r0 r1 => Boolean -> r0 -> r1
setFormFieldsTouched = hmap <<< SetFormFieldTouched


-- | Unwrap every newtype in a record filled with newtypes
data UnwrapField = UnwrapField

instance unwrapField :: (Newtype wrapper x) => Mapping UnwrapField wrapper x where
  mapping UnwrapField = unwrap

unwrapRecord :: ∀ r0 r1. HMap UnwrapField r0 r1 => r0 -> r1
unwrapRecord = hmap UnwrapField


-- | Wrap every field in a record with a particular newtype
data WrapField = WrapField

instance wrapField :: (Newtype wrapper x) => Mapping WrapField x wrapper where
  mapping WrapField = wrap

wrapRecord :: ∀ r0 r1. HMap WrapField r0 r1 => r0 -> r1
wrapRecord = hmap WrapField


----------
-- Traversals

-- TODO
-- formFieldsToMaybeOutputFields :: { | fields } -> Maybe { | outputs }


----------
-- Helpers

-- | Apply a builder that produces an output record from an empty record
fromScratch :: ∀ r. FromScratch r -> Record r
fromScratch = Builder.build <@> {}

-- | Perform a traversal
