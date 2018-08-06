-- | Various helpers to make working with validation based on
-- | `Data.Validation.Semigroup` nicer when using Formless.
module Formless.Validation.Semigroup where

import Prelude

import Data.Either (Either)
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as Validation

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

