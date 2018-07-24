-- | Various helpers to make working with validation based on
-- | `Polyform.Validation` nicer when using Formless.
module Formless.Validation.Polyform where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Internal as Internal
import Formless.Spec (InputField(..))
import Polyform.Validation (V(..), Validation, runValidation)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

-- | Turn a Polyform `Validation` validator into one that operates on
-- | an `InputField` directly. Does not apply validation to fields unless
-- | their .touched field equals `true`.
-- |
-- | ```purescript
-- | -- This will validate the input field and set the result field.
-- | { name: validateNonEmpty `onInputField` form.name
-- | , email :: validateEmailRegex `onInputField` form.email
-- | }
-- | ```
onInputField
  :: ∀ m e i o
   . Monad m
  => Validation m e i o
  -> InputField e i o
  -> m (InputField e i o)
onInputField validator field@(InputField i)
  | not i.touched = pure field
  | otherwise = do
      res <- runValidation validator i.input
      pure $ InputField $ case res of
        Invalid e -> i { result = Just $ Left e }
        Valid _ a -> i { result = Just $ Right a }


-- | A function to transform a record of `Validation` types accepting a
-- | particular input type into one that will operate on `InputField`s with
-- | the same input, error, and output types.
-- |
-- | Type inference is not so good with this function. Input types have to
-- | be annotated if they are not concrete.
-- |
-- | ```purescript
-- | validator :: Form InputField -> m (Form InputField)
-- | validator = applyOnInputFields
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnInputFields
  :: ∀ form form' fvxs fv io i o oxs o' m
   . Monad m
  => RL.RowToList fv fvxs
  => RL.RowToList o oxs
  => OnInputFields fvxs fv io
  => Internal.ApplyRecord io i o
  => Internal.SequenceRecord oxs o o' m
  => Newtype (form (Validation m)) (Record fv)
  => Newtype (form InputField) (Record i)
  => Newtype (form' InputField) (Record o')
  => Record fv
  -> form InputField
  -> m (form' InputField)
applyOnInputFields r = map wrap <<< Internal.sequenceRecord <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Internal.fromScratch (onInputFieldsBuilder (RLProxy :: RLProxy fvxs) r)

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnInputFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onInputFieldsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onInputFieldsNil :: OnInputFields RL.Nil row () where
  onInputFieldsBuilder _ _ = identity

instance onInputFieldsCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (Validation m e i o) trash row
     , OnInputFields tail row from
     , Internal.Row1Cons name (InputField e i o -> m (InputField e i o)) from to
     )
  => OnInputFields (RL.Cons name (Validation m e i o) tail) row to where
  onInputFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onInputField $ Record.get _name r
      rest = onInputFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func

