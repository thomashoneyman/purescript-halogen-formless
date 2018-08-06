module Formless.Spec where

import Prelude

import Data.Either (Either)
import Data.Lens (Lens', preview, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row

-- | @monoidmusician
data FormProxy
  ( form
      :: (# Type -> Type)
      -> (Type -> Type -> Type -> Type)
      -> Type
  ) = FormProxy

-- | A wrapper to represent the validation function on a form field
newtype Validator m error input output = Validator (input -> m (Either error output))
derive instance newtypeValidator :: Newtype (Validator m e i o) _

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField error input output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField e i o) _

-- | A wrapper to represent only the input type.
newtype InputField error input output = InputField input
derive instance newtypeInputField :: Newtype (InputField e i o) _
derive newtype instance eqInputField :: Eq i => Eq (InputField e i o)
derive newtype instance ordInputField :: Ord i => Ord (InputField e i o)

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype FormField m e i o = FormField (Record (FormFieldRow m e i o))
derive instance newtypeFormField :: Newtype (FormField m e i o) _

-- | The row used for the FormField newtype and in lens type signatures
type FormFieldRow m error input output =
  ( input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output)
  , validator :: Maybe (input -> m (Either error output))
  )

-- | Proxies for each of the fields in FormField for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_result = SProxy :: SProxy "result"
_validator = SProxy :: SProxy "validator"


----------
-- Conveniences

getField
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> Record (FormFieldRow m e i o)
getField sym = view (_Field sym)

getInput
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> i
getInput sym = view (_Input sym)

getResult
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> Maybe (Either e o)
getResult sym = view (_Result sym)

getError
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> Maybe e
getError sym = preview (_Error sym)

getOutput
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> Maybe o
getOutput sym = preview (_Output sym)

getTouched
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> Boolean
getTouched sym = view (_Touched sym)


----------
-- Lenses

-- | Easy access to any given field from the form, unwrapped
_Field
  :: ∀ sym form t0 fields e i o m
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) (Record (FormFieldRow m e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | Easy access to any given field's input value from the form
_Input
  :: ∀ sym form t0 fields e i o m
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) i
_Input sym = _Field sym <<< prop _input

-- | Easy access to any given field's touched value from the form
_Touched
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) Boolean
_Touched sym = _Field sym <<< prop _touched

-- | Easy access to any given field's result value from the form, if the
-- | result exists.
_Result
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) (Maybe (Either e o))
_Result sym = _Field sym <<< prop _result

-- | Easy access to any given field's error from its result field in the form,
-- | if the error exists.
_Error
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record (FormField m)) e
_Error sym = _Result sym <<< _Just <<< _Left

-- | Easy access to any given field's output from its result field in the form,
-- | if the output exists.
_Output
  :: ∀ sym form t0 fields m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record (FormField m)) o
_Output sym = _Result sym <<< _Just <<< _Right


----------
-- Helper types

-- | A type synonym that lets you pick out just the error type from
-- | your form row.
type ErrorType error input output = error

-- | A type synonym that lets you pick out just the input type from
-- | your form row.
type InputType error input output = input

-- | A type synonym that lets you pick out just the output type from
-- | your form row.
type OutputType error input output = output
