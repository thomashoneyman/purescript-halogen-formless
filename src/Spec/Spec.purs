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
import Prim.Row as Row

-- | Create a proxy for your form type, for use with functions that generate records from
-- | form proxies.
data FormProxy (form :: (# Type -> Type) -> (Type -> Type -> Type -> Type) -> Type) = FormProxy

-- | A wrapper to represent only the input type. Requires that `eq` is defined for the input
-- | type in order to track dirty states.
newtype InputField error input output = InputField input
derive instance newtypeInputField :: Newtype (InputField e i o) _
derive newtype instance eqInputField :: Eq i => Eq (InputField e i o)

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField error input output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField e i o) _
derive newtype instance eqOutputField :: Eq o => Eq (OutputField e i o)

-- | Represents a unit value with the correct number of arguments; largely for internal use.
data U e i o = U

-- | The type that we need to record state across the form
newtype FormField e i o = FormField (Record (FormFieldRow e i o))
derive instance newtypeFormField :: Newtype (FormField e i o) _
derive newtype instance eqFormField :: (Eq e, Eq i, Eq o) => Eq (FormField e i o)

-- | The row used for the FormField newtype and in lens type signatures
type FormFieldRow error input output =
  ( input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output)
  )

-- | Proxy for the 'input' field of a form field
_input = SProxy :: SProxy "input"

-- | Proxy for the 'touched' field of a form field
_touched = SProxy :: SProxy "touched"

-- | Proxy for the 'result' field of a form field
_result = SProxy :: SProxy "result"


----------
-- Helpers

-- | A type synonym that lets you pick out just the error type from
-- | your form row.
type ErrorType error input output = error

-- | A type synonym that lets you pick out just the input type from
-- | your form row.
type InputType error input output = input

-- | A type synonym that lets you pick out just the output type from
-- | your form row.
type OutputType error input output = output

----------
-- Lenses

type FormFieldLens e i o x =
  ∀ sym form fields t0
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record FormField) x

-- | A lens to operate on the field at a given symbol in your form
_Field :: ∀ e i o. FormFieldLens e i o (Record (FormFieldRow e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | A lens to operate on the input at a given symbol in your form
_Input :: ∀ e i o. FormFieldLens e i o i
_Input sym = _Field sym <<< prop _input

-- | A lens to operate on the 'touched' field at a given symbol in your form
_Touched :: ∀ e i o. FormFieldLens e i o Boolean
_Touched sym = _Field sym <<< prop _touched

-- | A lens to operate on the 'result' field at a given symbol in your form
_Result :: ∀ e i o. FormFieldLens e i o (Maybe (Either e o))
_Result sym = _Field sym <<< prop _result

-- | A traversal to operate on the possible error inside the 'result' field at
-- | a given symbol in your form
_Error
  :: ∀ sym form fields t0 e i o
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record FormField) e
_Error sym = _Result sym <<< _Just <<< _Left

-- | A traversal to operate on the possible output inside the 'result' field at
-- | a given symbol in your form
_Output
  :: ∀ sym form fields t0 e i o
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record FormField) o
_Output sym = _Result sym <<< _Just <<< _Right

