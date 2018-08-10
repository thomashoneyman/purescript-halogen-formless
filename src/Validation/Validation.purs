module Formless.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)

----------
-- Helpers

-- | Used to take a pure i -> o function and turn it into a correct Validation
hoistFn :: ∀ m e i o. Monad m => (i -> o) -> Validation m e i o
hoistFn f = Validation $ f >>> pure >>> pure

-- | Used to take a pure i -> V e o function and turn it into a correct Validation
hoistFnE :: ∀ m e i o. Monad m => (i -> Either e o) -> Validation m e i o
hoistFnE f = Validation $ f >>> pure

----------
-- Core type

-- | A wrapper to represent the validation function on a form field.
-- | Directly inspired by the Validation type from purescript-polyform
-- | Thanks to @paluh
newtype Validation m error input output = Validation (input -> m (Either error output))
derive instance newtypeValidation :: Newtype (Validation m e i o) _
derive instance functorValidation :: Functor m => Functor (Validation m e i)

instance applyValidation :: Monad m => Apply (Validation m e i) where
  apply vf va = Validation \i -> do
    vf' <- unwrap vf i
    va' <- unwrap va i
    pure $ vf' <*> va'

instance applicativeValidation :: Monad m => Applicative (Validation m e i) where
  pure = Validation <<< const <<< pure <<< pure

instance altValidation :: Monad m => Alt (Validation m e i) where
  alt v0 v1 = Validation \i -> do
    v0' <- unwrap v0 i
    v1' <- unwrap v1 i
    pure $ v0' <|> v1'

instance semigroupValidation :: Semigroup (m (Either e o)) => Semigroup (Validation m e i o) where
  append (Validation v0) (Validation v1) = Validation \i -> v0 i <> v1 i

instance monoidValidation
  :: (Applicative m, Monoid (Either e o), Semigroup (m (Either e o)))
  => Monoid (Validation m e i o) where
  mempty = Validation <<< const <<< pure $ mempty

instance semigroupoidValidation :: Monad m => Semigroupoid (Validation m e) where
  compose v1 v0 = Validation \i -> do
    eo <- unwrap v0 i
    either (pure <<< Left) (unwrap v1) eo

instance categoryValidation :: Monad m => Category (Validation m e) where
  identity = Validation $ pure <<< pure

instance profunctorValidation :: Monad m => Profunctor (Validation m e) where
  dimap l r v = (Validation $ l >>> pure >>> pure) >>> v >>> (Validation $ r >>> pure >>> pure)

