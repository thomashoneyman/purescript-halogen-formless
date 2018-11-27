module Formless.Types.Form where

import Prelude

import Data.Newtype (class Newtype)
import Formless.Data.FormFieldResult (FormFieldResult)

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

-- | Represents modifications to input fields
newtype InputFunction error input output = InputFunction (input -> input)
derive instance newtypeInputFunction :: Newtype (InputFunction e i o) _

-- | The type that we need to record state across the form
newtype FormField e i o = FormField (Record (FormFieldRow e i o))
derive instance newtypeFormField :: Newtype (FormField e i o) _
derive instance eqFormField :: (Eq e, Eq i, Eq o) => Eq (FormField e i o)

-- | The row used for the FormField newtype and in lens type signatures
type FormFieldRow error input output =
  ( input :: input
  , touched :: Boolean
  , result :: FormFieldResult error output
  )

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
