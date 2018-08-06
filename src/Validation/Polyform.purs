-- | Various helpers to make working with validation based on
-- | `Polyform.Validation` nicer when using Formless.
module Formless.Validation.Polyform where

import Prelude

import Data.Either (Either)
import Polyform.Validation (Validation(..))
import Polyform.Validation as Validation

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
