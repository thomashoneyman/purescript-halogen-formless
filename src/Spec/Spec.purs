module Formless.Spec where

import Prelude

import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Cons)

-- | @monoidmusician
data FormProxy (form :: (# Type -> Type) -> (Type -> Type -> Type -> Type) -> Type) = FormProxy

-- | The type that will be applied to the user's input row to
-- | create the spec form that we'll compare against to measure
-- | 'touched' states, etc. This is what the user is responsible
-- | for providing.
newtype FormSpec error input output = FormSpec input
derive instance newtypeFormSpec :: Newtype (FormSpec e i o) _

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField error input output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField e i o) _

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField e i o = InputField (Record (InputFieldRow e i o))
derive instance newtypeInputField :: Newtype (InputField e i o) _

-- | The row used for the InputField newtype and in lens type signatures
type InputFieldRow error input output =
  ( input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output)
  )

-- | Proxies for each of the fields in InputField and FormSpec for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_result = SProxy :: SProxy "result"

----------
-- Lenses

-- | Easy access to any given field from the form, unwrapped
_Field
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record InputField) (Record (InputFieldRow e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | Easy access to any given field's input value from the form
_Input
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record InputField) i
_Input sym = _Field sym <<< prop _input

-- | Easy access to any given field's touched value from the form
_Touched
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record InputField) Boolean
_Touched sym = _Field sym <<< prop _touched

-- | Easy access to any given field's result value from the form, if the
-- | result exists.
_Result
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record InputField) (Maybe (Either e o))
_Result sym = _Field sym <<< prop _result

-- | Easy access to any given field's error from its result field in the form,
-- | if the error exists.
_Error
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record InputField) e
_Error sym = _Result sym <<< _Just <<< _Left

-- | Easy access to any given field's output from its result field in the form,
-- | if the output exists.
_Output
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form Record InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record InputField) o
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

