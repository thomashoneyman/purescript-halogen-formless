-- | Various helpers to make working with validation based on
-- | `Polyform.Validation` nicer when using Formless.
module Formless.Validation.Polyform where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Internal as Internal
import Formless.Spec (FormInput(..))
import Polyform.Validation (V(..), Validation, runValidation)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

-- | Turn a Polyform `Validation` validator into one that operates on
-- | an `FormInput` directly. Does not apply validation to fields unless
-- | their .touched field equals `true`.
-- |
-- | ```purescript
-- | -- This will validate the input field and set the result field.
-- | { name: validateNonEmpty `onFormInput` form.name
-- | , email :: validateEmailRegex `onFormInput` form.email
-- | }
-- | ```
onFormInput
  :: ∀ m e i o
   . Monad m
  => Validation m e i o
  -> FormInput e i o
  -> m (FormInput e i o)
onFormInput validator field@(FormInput i)
  | not i.touched = pure field
  | otherwise = do
      res <- runValidation validator i.input
      pure $ FormInput $ case res of
        Invalid e -> i { result = Just $ Left e }
        Valid _ a -> i { result = Just $ Right a }


-- | A function to transform a record of `Validation` types accepting a
-- | particular input type into one that will operate on `FormInput`s with
-- | the same input, error, and output types.
-- |
-- | Type inference is not so good with this function. Input types have to
-- | be annotated if they are not concrete.
-- |
-- | ```purescript
-- | validator :: Form FormInput -> m (Form FormInput)
-- | validator = applyOnFormInputs
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnFormInputs
  :: ∀ form form' fvxs fv io i o oxs o' m
   . Monad m
  => RL.RowToList fv fvxs
  => RL.RowToList o oxs
  => OnFormInputs fvxs fv io
  => Internal.ApplyRecord io i o
  => Internal.SequenceRecord oxs o o' m
  => Newtype (form Record (Validation m)) (Record fv)
  => Newtype (form Record FormInput) (Record i)
  => Newtype (form' Record FormInput) (Record o')
  => Record fv
  -> form Record FormInput
  -> m (form' Record FormInput)
applyOnFormInputs r = map wrap <<< Internal.sequenceRecord <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Internal.fromScratch (onFormInputsBuilder (RLProxy :: RLProxy fvxs) r)

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnFormInputs (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onFormInputsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onFormInputsNil :: OnFormInputs RL.Nil row () where
  onFormInputsBuilder _ _ = identity

instance onFormInputsCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (Validation m e i o) trash row
     , OnFormInputs tail row from
     , Internal.Row1Cons name (FormInput e i o -> m (FormInput e i o)) from to
     )
  => OnFormInputs (RL.Cons name (Validation m e i o) tail) row to where
  onFormInputsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onFormInput $ Record.get _name r
      rest = onFormInputsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func

