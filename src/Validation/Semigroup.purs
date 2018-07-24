-- | Various helpers to make working with validation based on
-- | `Data.Validation.Semigroup` nicer when using Formless.
module Formless.Validation.Semigroup where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Validation.Semigroup (V, unV)
import Formless.Internal as Internal
import Formless.Spec (InputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

-- | Turn a `V` validator into one that operates on an InputField
-- | directly. Does not apply validation to fields unless their
-- | .touched field is true.
-- |
-- | ```purescript
-- | -- This will validate the input field and set the result field.
-- | { name: validateNonEmpty `onInputField` form.name
-- | , email :: validateEmailRegex `onInputField` form.email }
-- | ```
onInputField
  :: ∀ i e o
   . (i -> V e o)
  -> InputField e i o
  -> InputField e i o
onInputField validator field@(InputField i)
  | not i.touched = field
  | otherwise = InputField $ unV
      (\e -> i { result = Just $ Left e })
      (\v -> i { result = Just $ Right v })
      (validator i.input)

-- | A function to transform a record of validation functions accepting a
-- | particular input type into one that will operate on `InputField`s with
-- | the same input, error, and output types.
-- |
-- | Type inference is not so good with this function. Input types have to
-- | be annotated if they are not concrete.
-- |
-- | ```purescript
-- | -- Unable to verify what 'x' is, so you'll need to annotate.
-- | validateNonEmptyArray :: Array x -> V Error (Array x)
-- | validateNonEmptyArray = ...
-- |
-- | validator :: Form InputField -> Form InputField
-- | validator = applyOnInputFields
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnInputFields
  :: ∀ form form' fvxs fv io i o
   . RL.RowToList fv fvxs
  => OnInputFields fvxs fv io
  => Internal.ApplyRecord io i o
  => Newtype (form InputField) (Record i)
  => Newtype (form' InputField) (Record o)
  => Record fv
  -> form InputField
  -> form' InputField
applyOnInputFields r = wrap <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Builder.build (onInputFieldsBuilder (RLProxy :: RLProxy fvxs) r) {}

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnInputFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onInputFieldsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onInputFieldsNil :: OnInputFields RL.Nil row () where
  onInputFieldsBuilder _ _ = identity

instance onInputFieldsCons
  :: ( IsSymbol name
     , Row.Cons name (i -> V e o) trash row
     , OnInputFields tail row from
     , Internal.Row1Cons name (InputField e i o -> InputField e i o) from to
     )
  => OnInputFields (RL.Cons name (i -> V e o) tail) row to where
  onInputFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onInputField $ Record.get _name r
      rest = onInputFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func

