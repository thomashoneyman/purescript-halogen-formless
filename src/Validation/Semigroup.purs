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
import Formless.Spec (FormInput(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

-- | Turn a `V` validator into one that operates on an FormInput
-- | directly. Does not apply validation to fields unless their
-- | .touched field is true.
-- |
-- | ```purescript
-- | -- This will validate the input field and set the result field.
-- | { name: validateNonEmpty `onFormInput` form.name
-- | , email :: validateEmailRegex `onFormInput` form.email }
-- | ```
onFormInput
  :: ∀ i e o
   . (i -> V e o)
  -> FormInput e i o
  -> FormInput e i o
onFormInput validator field@(FormInput i)
  | not i.touched = field
  | otherwise = FormInput $ unV
      (\e -> i { result = Just $ Left e })
      (\v -> i { result = Just $ Right v })
      (validator i.input)

-- | A function to transform a record of validation functions accepting a
-- | particular input type into one that will operate on `FormInput`s with
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
-- | validator :: Form FormInput -> Form FormInput
-- | validator = applyOnFormInputs
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnFormInputs
  :: ∀ form form' fvxs fv io i o
   . RL.RowToList fv fvxs
  => OnFormInputs fvxs fv io
  => Internal.ApplyRecord io i o
  => Newtype (form Record FormInput) (Record i)
  => Newtype (form' Record FormInput) (Record o)
  => Record fv
  -> form Record FormInput
  -> form' Record FormInput
applyOnFormInputs r = wrap <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Builder.build (onFormInputsBuilder (RLProxy :: RLProxy fvxs) r) {}

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnFormInputs (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onFormInputsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onFormInputsNil :: OnFormInputs RL.Nil row () where
  onFormInputsBuilder _ _ = identity

instance onFormInputsCons
  :: ( IsSymbol name
     , Row.Cons name (i -> V e o) trash row
     , OnFormInputs tail row from
     , Internal.Row1Cons name (FormInput e i o -> FormInput e i o) from to
     )
  => OnFormInputs (RL.Cons name (i -> V e o) tail) row to where
  onFormInputsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onFormInput $ Record.get _name r
      rest = onFormInputsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func

