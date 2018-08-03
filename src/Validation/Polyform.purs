-- | Various helpers to make working with validation based on
-- | `Polyform.Validation` nicer when using Formless.
module Formless.Validation.Polyform where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Internal as Internal
import Formless.Spec (FormField(..))
import Polyform.Validation (Validation(..))
import Polyform.Validation as Validation
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

----------
-- Fields

-- | Turn a Polyform `Validation` validator into one that is compatible
-- | with the field-level validator expected by Formless
toEither
  :: ∀ m e i o
   . Monad m
  => Validation m e i o
  -> (i -> m (Either e o))
toEither (Validation f) = (map <<< map) Validation.toEither f

----------
-- Full-Form

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
onFormField
  :: ∀ m e i o
   . Monad m
  => Validation m e i o
  -> FormField m e i o
  -> m (FormField m e i o)
onFormField validator field@(FormField i)
  | not i.touched = pure field
  | otherwise = do
      res <- Validation.runValidation validator i.input
      pure $ FormField $ case res of
        Validation.Invalid e -> i { result = Just $ Left e }
        Validation.Valid _ a -> i { result = Just $ Right a }

-- | A function to transform a record of `Validation` types accepting a
-- | particular input type into one that will operate on `FormField`s with
-- | the same input, error, and output types.
-- |
-- | Type inference is not so good with this function. Input types have to
-- | be annotated if they are not concrete.
-- |
-- | ```purescript
-- | validator :: Form FormField -> m (Form FormField)
-- | validator = applyOnFormFields
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnFormFields
  :: ∀ form form' fvxs fv io i o oxs o' m
   . Monad m
  => RL.RowToList fv fvxs
  => RL.RowToList o oxs
  => OnFormFields fvxs fv io
  => Internal.ApplyRecord io i o
  => Internal.SequenceRecord oxs o o' m
  => Newtype (form Record (Validation m)) (Record fv)
  => Newtype (form Record (FormField m)) (Record i)
  => Newtype (form' Record (FormField m)) (Record o')
  => Record fv
  -> form Record (FormField m)
  -> m (form' Record (FormField m))
applyOnFormFields r = map wrap <<< Internal.sequenceRecord <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Internal.fromScratch (onFormFieldsBuilder (RLProxy :: RLProxy fvxs) r)

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnFormFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onFormFieldsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onFormFieldsNil :: OnFormFields RL.Nil row () where
  onFormFieldsBuilder _ _ = identity

instance onFormFieldsCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (Validation m e i o) trash row
     , OnFormFields tail row from
     , Internal.Row1Cons name (FormField m e i o -> m (FormField m e i o)) from to
     )
  => OnFormFields (RL.Cons name (Validation m e i o) tail) row to where
  onFormFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onFormField $ Record.get _name r
      rest = onFormFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func

