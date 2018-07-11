module Example.Validation.Utils where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe(..))

-- | Unpacks errors to render as a string
showError
  :: ∀ e o r
   . Show e
  => { result :: Maybe (Either e o) | r }
  -> Maybe String
showError = join <<< map (either (Just <<< show) (const Nothing)) <<< _.result

