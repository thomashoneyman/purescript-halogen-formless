-- | Various helpers to make working with validation based on
-- | `Data.Validation.Semigroup` nicer when using Formless.
module Formless.Validation.Semigroup where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Validation.Semigroup (V, unV)
import Data.Validation.Semigroup as Validation
import Formless.Internal as Internal
import Formless.Spec (FormField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))

----------
-- Field Level

toEitherPure
  :: ∀ m e i o
   . Applicative m
  => (i -> V e o)
  -> (i -> m (Either e o))
toEitherPure = map (pure <<< Validation.toEither)

toEither
  :: ∀ m e i o
   . Functor m
  => (i -> m (V e o))
  -> (i -> m (Either e o))
toEither = (map <<< map) Validation.toEither

----------
-- Full Form

-- | Turn a `V` validator into one that operates on a FormField
-- | directly. Does not apply validation to fields unless their
-- | .touched field is true.
-- |
-- | ```purescript
-- | -- This will validate the input field and set the result field.
-- | { name: validateNonEmpty `onFormField` form.name
-- | , email :: validateEmailRegex `onFormField` form.email }
-- | ```
onFormField
  :: ∀ m e i o
   . Monad m
  => (i -> V e o)
  -> FormField m e i o
  -> FormField m e i o
onFormField validator field@(FormField i)
  | not i.touched = field
  | otherwise = FormField $ unV
      (\e -> i { result = Just $ Left e })
      (\v -> i { result = Just $ Right v })
      (validator i.input)

-- | A function to transform a record of validation functions accepting a
-- | particular input type into one that will operate on `FormField`s with
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
-- | validator :: Form FormField -> Form FormField
-- | validator = applyOnFormFields
-- |  { name: validateNonEmptyString
-- |  , email: validateEmailRegex
-- |  , dates: \(i :: Array String) -> validateNonEmptyArray i }
-- | ```
applyOnFormFields
  :: ∀ form form' fvxs fv io m i o
   . RL.RowToList fv fvxs
  => OnFormFields fvxs fv io
  => Internal.ApplyRecord io i o
  => Newtype (form Record (FormField m)) (Record i)
  => Newtype (form' Record (FormField m)) (Record o)
  => Record fv
  -> form Record (FormField m)
  -> form' Record (FormField m)
applyOnFormFields r = wrap <<< Internal.applyRecord io <<< unwrap
  where
    io :: Record io
    io = Builder.build (onFormFieldsBuilder (RLProxy :: RLProxy fvxs) r) {}

-- | The class that provides the Builder implementation to efficiently unpack a record of
-- | output fields into a simple record of only the values.
class OnFormFields (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  onFormFieldsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance onFormFieldsNil :: OnFormFields RL.Nil row () where
  onFormFieldsBuilder _ _ = identity

instance onFormFieldsCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (i -> V e o) trash row
     , OnFormFields tail row from
     , Internal.Row1Cons name (FormField m e i o -> FormField m e i o) from to
     )
  => OnFormFields (RL.Cons name (i -> V e o) tail) row to where
  onFormFieldsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      func = onFormField $ Record.get _name r
      rest = onFormFieldsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name func
--
