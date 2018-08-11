module Formless.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)

----------
-- Helpers

-- | A more verbose but clearer function for running a validation function on its inputs
runValidation :: ∀ st m e i o. Monad m => Validation st m e i o -> st -> i -> m (Either e o)
runValidation = unwrap

-- | Used to take a pure i -> o function and turn it into a correct Validation
hoistFn :: ∀ st m e i o. Monad m => (i -> o) -> Validation st m e i o
hoistFn f = Validation $ \_ -> f >>> pure >>> pure

-- | Used to take a pure i -> V e o function and turn it into a correct Validation
hoistFnE :: ∀ st m e i o. Monad m => (i -> Either e o) -> Validation st m e i o
hoistFnE f = Validation $ \_ -> f >>> pure

----------
-- Core type

-- | A wrapper to represent the validation function on a form field, which can itself take
-- | the form state as its first argument. Inspired in some parts by the Validation type
-- | from purescript-polyform by @paluh.
newtype Validation st m error input output = Validation (st -> input -> m (Either error output))
derive instance newtypeValidation :: Newtype (Validation st m e i o) _
derive instance functorValidation :: Functor m => Functor (Validation st m e i)

instance applyValidation :: Monad m => Apply (Validation st m e i) where
  apply vf va = Validation \st i -> do
    vf' <- unwrap vf st i
    va' <- unwrap va st i
    pure $ vf' <*> va'

instance applicativeValidation :: Monad m => Applicative (Validation st m e i) where
  pure = Validation <<< const <<< const <<< pure <<< pure

instance altValidation :: Monad m => Alt (Validation st m e i) where
  alt v0 v1 = Validation \st i -> do
    v0' <- unwrap v0 st i
    v1' <- unwrap v1 st i
    pure $ v0' <|> v1'

instance semigroupValidation :: Semigroup (m (Either e o)) => Semigroup (Validation st m e i o) where
  append (Validation v0) (Validation v1) = Validation \st i -> v0 st i <> v1 st i

instance monoidValidation
  :: (Applicative m, Monoid (m (Either e o)), Semigroup (m (Either e o)))
  => Monoid (Validation st m e i o) where
  mempty = Validation <<< const <<< pure $ mempty

instance semigroupoidValidation :: Monad m => Semigroupoid (Validation st m e) where
  compose v1 v0 = Validation \st i -> do
    eo <- unwrap v0 st i
    either (pure <<< Left) (unwrap v1 st) eo

instance categoryValidation :: Monad m => Category (Validation st m e) where
  identity = Validation $ \_ -> pure <<< pure

