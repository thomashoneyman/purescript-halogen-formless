-- | A module with functions for retriving particular fields from a form
module Formless.Retrieve where

import Prelude

import Data.Lens (Lens', preview, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Data.FormFieldResult (FormFieldResult(..), _Error, _Success, toMaybe)
import Formless.Types.Form (FormField(..), FormFieldRow)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row

----------
-- Fields

-- | Given a form, get the field at the specified symbol
getField :: ∀ e i o. FormFieldGet e i o (Record (FormFieldRow e i o))
getField sym = view (_Field sym)

-- | Given a form, get the input at the specified symbol
getInput :: ∀ e i o. FormFieldGet e i o i
getInput sym = view (_FieldInput sym)

-- | Given a form, get the touched field at the specified symbol
getTouched :: ∀ e i o. FormFieldGet e i o Boolean
getTouched sym = view (_FieldTouched sym)

-- | Given a form, get the result at the specified symbol
getResult :: ∀ e i o. FormFieldGet e i o (FormFieldResult e o)
getResult sym = view (_FieldResult sym)

-- | Given a form, get the error (if it exists) at the specified symbol
getError :: ∀ e i o. FormFieldGet e i o (Maybe e)
getError sym = preview (_FieldError sym)

-- | Given a form, get the output (if it exists) at the specified symbol
getOutput :: ∀ e i o. FormFieldGet e i o (Maybe o)
getOutput sym = preview (_FieldOutput sym)

----------
-- Summary functions

-- | Get the form as a record where all fields are only the input value
getInputAll :: GetAll GetInputField
getInputAll = hmap GetInputField <<< unwrap

-- | Get the form as a record where all fields are only the touched value
getTouchedAll :: GetAll GetTouchedField
getTouchedAll = hmap GetTouchedField <<< unwrap

-- | Get the form as a record where all fields are only the result value
getResultAll :: GetAll GetResultField
getResultAll = hmap GetResultField <<< unwrap

-- | Get the form as a record where all fields are only the error value
getErrorAll :: GetAll GetError
getErrorAll = hmap GetError <<< unwrap

-- | Get the form as a record where all fields are only the output value
getOutputAll :: GetAll GetOutput
getOutputAll = hmap GetOutput <<< unwrap

----------
-- Lenses

-- | A lens to operate on the field at a given symbol in your form
_Field :: ∀ e i o. FormFieldLens e i o (Record (FormFieldRow e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | A lens to operate on the input at a given symbol in your form
_FieldInput :: ∀ e i o. FormFieldLens e i o i
_FieldInput sym = _Field sym <<< prop (SProxy :: SProxy "input")

-- | A lens to operate on the 'touched' field at a given symbol in your form
_FieldTouched :: ∀ e i o. FormFieldLens e i o Boolean
_FieldTouched sym = _Field sym <<< prop (SProxy :: SProxy "touched")

-- | A lens to operate on the 'result' field at a given symbol in your form
_FieldResult :: ∀ e i o. FormFieldLens e i o (FormFieldResult e o)
_FieldResult sym = _Field sym <<< prop (SProxy :: SProxy "result")

-- | A traversal to operate on the possible error inside the 'result' field at
-- | a given symbol in your form
_FieldError
  :: ∀ sym form fields t0 e i o
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record FormField) e
_FieldError sym = _FieldResult sym <<< _Error

-- | A traversal to operate on the possible output inside the 'result' field at
-- | a given symbol in your form
_FieldOutput
  :: ∀ sym form fields t0 e i o
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record FormField) o
_FieldOutput sym = _FieldResult sym <<< _Success

----------
-- Types

-- | A type representing a function to produce a value from a record of
-- | form fields given a particular symbol. The result of `view` from
-- | Data.Lens applied with a particular lens to the form.
type FormFieldGet e i o x =
  ∀ sym form fields t0
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> form Record FormField
  -> x

-- | A type representing a lens onto part of a form field
type FormFieldLens e i o x =
  ∀ sym form fields t0
   . IsSymbol sym
  => Newtype (form Record FormField) (Record fields)
  => Row.Cons sym (FormField e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record FormField) x

-- | A type representing retrieving all of a particular field with the field's
-- | constructor name. For internal use.
type GetAll f =
  ∀ form r0 r1
   . HMap f r0 r1
  => Newtype (form Record FormField) r0
  => form Record FormField
  -> r1

-- | Data constructor for the getInputField function
data GetInputField = GetInputField

-- | Heterogeneous type class for the getInputField function
instance getInputField :: Mapping GetInputField (FormField e i o) i where
  mapping GetInputField (FormField { input }) = input

-- | Data constructor for the getTouchedField function
data GetTouchedField = GetTouchedField

-- | Heterogeneous type class for the getTouchedField function
instance getTouchedField :: Mapping GetTouchedField (FormField e i o) Boolean where
  mapping GetTouchedField (FormField { touched }) = touched

-- | Data constructor for the getResultField function
data GetResultField = GetResultField

-- | Heterogeneous type class for the getResultField function
instance getResultField :: Mapping GetResultField (FormField e i o) (FormFieldResult e o) where
  mapping GetResultField (FormField { result }) = result

-- | Data constructor for the getError function
data GetError = GetError

-- | Heterogeneous type class for the getError function
instance getErrorResult' :: Mapping GetError (FormField e i o) (Maybe e) where
  mapping GetError (FormField { result }) = case result of
    Error e -> Just e
    _ -> Nothing

-- | Data constructor for the getOutput function
data GetOutput = GetOutput

-- | Heterogeneous type class for the getOutput function
instance getOutput' :: Mapping GetOutput (FormField e i o) (Maybe o) where
  mapping GetOutput (FormField { result }) = toMaybe result
