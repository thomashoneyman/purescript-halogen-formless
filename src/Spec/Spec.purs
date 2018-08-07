module Formless.Spec where

import Prelude

import Data.Either (Either)
import Data.Lens (Lens', view)
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
data FormProxy (form :: (# Type -> Type) -> (Type -> Type -> Type -> Type) -> Type) = FormProxy

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
-- Get

type FormFieldGet m e i o x =
  forall sym form fields t0
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> x

-- | Given a form, get the field at the specified symbol
getField :: ∀ m e i o. FormFieldGet m e i o (Record (FormFieldRow m e i o))
getField sym = view (_Field sym)

-- | Given a form, get the input at the specified symbol
getInput :: ∀ m e i o. FormFieldGet m e i o i
getInput sym = view (_Input sym)

-- | Given a form, get the result at the specified symbol
getResult :: ∀ m e i o. FormFieldGet m e i o (Maybe (Either e o))
getResult sym = view (_Result sym)


----------
-- Lenses

type FormFieldLens m e i o x =
  forall sym form fields t0
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) x

-- | A lens to operate on the field at a given symbol in your form
_Field :: ∀ m e i o. FormFieldLens m e i o (Record (FormFieldRow m e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | A lens to operate on the input at a given symbol in your form
_Input :: ∀ m e i o. FormFieldLens m e i o i
_Input sym = _Field sym <<< prop _input

-- | A lens to operate on the 'touched' field at a given symbol in your form
_Touched :: ∀ m e i o. FormFieldLens m e i o Boolean
_Touched sym = _Field sym <<< prop _touched

-- | A lens to operate on the 'result' field at a given symbol in your form
_Result :: ∀ m e i o. FormFieldLens m e i o (Maybe (Either e o))
_Result sym = _Field sym <<< prop _result

-- | A traversal to operate on the possible error inside the 'result' field at
-- | a given symbol in your form
_Error
  :: ∀ sym form fields t0 m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record (FormField m)) e
_Error sym = _Result sym <<< _Just <<< _Left

-- | A traversal to operate on the possible output inside the 'result' field at
-- | a given symbol in your form
_Output
  :: ∀ sym form fields t0 m e i o
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
