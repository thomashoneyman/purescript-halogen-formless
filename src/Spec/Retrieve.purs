-- | A module with helper lenses and functions for retriving particular fields from a form
module Formless.Spec.Retrieve where

import Prelude

import Data.Either (Either, either, hush)
import Data.Lens (preview, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy)
import Formless.Spec (FormField(..), FormFieldRow, _Error, _Field, _Input, _Output, _Result, _Touched)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row

----------
-- Fields

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

-- | Given a form, get the field at the specified symbol
getField :: ∀ e i o. FormFieldGet e i o (Record (FormFieldRow e i o))
getField sym = view (_Field sym)

-- | Given a form, get the input at the specified symbol
getInput :: ∀ e i o. FormFieldGet e i o i
getInput sym = view (_Input sym)

-- | Given a form, get the touched field at the specified symbol
getTouched :: ∀ e i o. FormFieldGet e i o Boolean
getTouched sym = view (_Touched sym)

-- | Given a form, get the result at the specified symbol
getResult :: ∀ e i o. FormFieldGet e i o (Maybe (Either e o))
getResult sym = view (_Result sym)

-- | Given a form, get the error (if it exists) at the specified symbol
getError :: ∀ e i o. FormFieldGet e i o (Maybe e)
getError sym = preview (_Error sym)

-- | Given a form, get the output (if it exists) at the specified symbol
getOutput :: ∀ e i o. FormFieldGet e i o (Maybe o)
getOutput sym = preview (_Output sym)


----------
-- Summary functions

-- | A type representing retrieving all of a particular field with the field's
-- | constructor name. For internal use.
type GetAll f =
  ∀ form r0 r1
   . HMap f r0 r1
  => Newtype (form Record FormField) r0
  => form Record FormField
  -> r1


data GetInputField = GetInputField

instance getInputField :: Mapping GetInputField (FormField e i o) i where
  mapping GetInputField (FormField { input }) = input

-- | Get the form as a record where all fields are only the input value
getInputAll :: GetAll GetInputField
getInputAll = hmap GetInputField <<< unwrap


data GetTouchedField = GetTouchedField

instance getTouchedField :: Mapping GetTouchedField (FormField e i o) Boolean where
  mapping GetTouchedField (FormField { touched }) = touched

-- | Get the form as a record where all fields are only the touched value
getTouchedAll :: GetAll GetTouchedField
getTouchedAll = hmap GetTouchedField <<< unwrap


data GetResultField = GetResultField

instance getResultField :: Mapping GetResultField (FormField e i o) (Maybe (Either e o)) where
  mapping GetResultField (FormField { result }) = result

-- | Get the form as a record where all fields are only the result value
getResultAll :: GetAll GetResultField
getResultAll = hmap GetResultField <<< unwrap


data GetError = GetError

instance getErrorResult' :: Mapping GetError (FormField e i o) (Maybe e) where
  mapping GetError (FormField { result }) = join $ either Just (const Nothing) <$> result

-- | Get the form as a record where all fields are only the error value
getErrorAll :: GetAll GetError
getErrorAll = hmap GetError <<< unwrap


data GetOutput = GetOutput

instance getOutput' :: Mapping GetOutput (FormField e i o) (Maybe o) where
  mapping GetOutput (FormField { result }) = join $ hush <$> result

-- | Get the form as a record where all fields are only the output value
getOutputAll :: GetAll GetOutput
getOutputAll = hmap GetOutput <<< unwrap
