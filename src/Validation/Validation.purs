module Formless.Validation where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, unV)
import Formless.Spec (InputField(..))

-- | Turn a `V` validator into one that operates on InputField
-- | directly, not validating untouched fields.
onInputField
  :: ∀ i e o
   . (i -> V e o)
  -> InputField i e o
  -> InputField i e o
onInputField validator field@(InputField i)
  | not i.touched = field
  | otherwise = InputField $ unV
      (\e -> i { result = Just $ Left e })
      (\v -> i { result = Just $ Right v })
      (validator i.input)

