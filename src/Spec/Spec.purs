module Formless.Spec where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Cons)
import Record as Record

-- | The type that will be applied to the user's input row to
-- | create the spec form that we'll compare against to measure
-- | 'touched' states, etc. This is what the user is responsible
-- | for providing.
newtype FormSpec input error output = FormSpec input
derive instance newtypeFormSpec :: Newtype (FormSpec i e o) _

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField input error output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField i e o) _

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean -- Whether the field has been changed by the user
  , result :: Maybe (Either error output)
  }
derive instance newtypeInputField :: Newtype (InputField i e o) _

-- | Proxies for each of the fields in InputField and FormSpec for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_result = SProxy :: SProxy "result"

-- | Easy access to any given field from the form, unwrapped
getField
  :: ∀ sym form t0 field fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => Newtype (InputField i e o) field
  => SProxy sym
  -> form InputField
  -> field
getField sym form =
  unwrap $ Record.get sym $ unwrap form

-- | Easy access to any given field's input value from the form
getInput
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> i
getInput sym = _.input <<< getField sym

-- | Easy access to any given field's touched value from the form
getTouched
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Boolean
getTouched sym = _.touched <<< getField sym

-- | Easy access to any given field's result value from the form, if the
-- | result exists.
getResult
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe (Either e o)
getResult sym = _.result <<< getField sym

-- | Easy access to any given field's error from its result field in the form,
-- | if the error exists.
getError
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe e
getError sym form = case getResult sym form of
  Just (Left e) -> Just e
  _ -> Nothing

-- | Easy access to any given field's output from its result field in the form,
-- | if the output exists.
getOutput
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe o
getOutput sym = join <<< map hush <<< getResult sym


----------
-- Helper types

-- | A type synonym that lets you pick out just the input type from
-- | your form row.
type Input input error output = input

-- | A type synonym that lets you pick out just the error type from
-- | your form row.
type Error input error output = error

-- | A type synonym that lets you pick out just the output type from
-- | your form row.
type Output input error output = output
