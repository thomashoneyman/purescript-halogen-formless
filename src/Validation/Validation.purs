module Formless.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Formless.Spec (FormField)

----------
-- Helpers

-- | A more verbose but clearer function for running a validation function on its inputs
runValidation :: ∀ form m e i o. Monad m => Validation form m e i o -> form Record FormField -> i -> m (Either e o)
runValidation = unwrap

-- | Used to take a pure i -> o function and turn it into a correct Validation
hoistFn :: ∀ form m e i o. Monad m => (i -> o) -> Validation form m e i o
hoistFn f = Validation $ \_ -> f >>> pure >>> pure

-- | Used to take a pure i -> V e o function and turn it into a correct Validation
hoistFnE :: ∀ form m e i o. Monad m => (i -> Either e o) -> Validation form m e i o
hoistFnE f = Validation $ \_ -> f >>> pure

----------
-- Core type

-- | A wrapper to represent the validation function on a form field, which can itself take
-- | the form state as its first argument. Inspired in some parts by the Validation type
-- | from purescript-polyform by @paluh.
newtype Validation form m error input output = Validation (form Record FormField -> input -> m (Either error output))
derive instance newtypeValidation :: Newtype (Validation form m e i o) _
derive instance functorValidation :: Functor m => Functor (Validation form m e i)

instance applyValidation :: Monad m => Apply (Validation form m e i) where
  apply vf va = Validation \form i -> do
    vf' <- unwrap vf form i
    va' <- unwrap va form i
    pure $ vf' <*> va'

instance applicativeValidation :: Monad m => Applicative (Validation form m e i) where
  pure = Validation <<< const <<< const <<< pure <<< pure

instance altValidation :: Monad m => Alt (Validation form m e i) where
  alt v0 v1 = Validation \form i -> do
    v0' <- unwrap v0 form i
    v1' <- unwrap v1 form i
    pure $ v0' <|> v1'

instance semigroupValidation :: Semigroup (m (Either e o)) => Semigroup (Validation form m e i o) where
  append (Validation v0) (Validation v1) = Validation \form i -> v0 form i <> v1 form i

instance monoidValidation
  :: (Applicative m, Monoid (m (Either e o)), Semigroup (m (Either e o)))
  => Monoid (Validation form m e i o) where
  mempty = Validation <<< const <<< pure $ mempty

instance semigroupoidValidation :: Monad m => Semigroupoid (Validation form m e) where
  compose v1 v0 = Validation \form i -> do
    eo <- unwrap v0 form i
    either (pure <<< Left) (unwrap v1 form) eo

instance categoryValidation :: Monad m => Category (Validation form m e) where
  identity = Validation $ \_ -> pure <<< pure

