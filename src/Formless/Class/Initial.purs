module Formless.Class.Initial where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class MonoidRecord)
import Data.Tuple (Tuple(..))
import Prim.RowList (class RowToList) as RL

-- | A type class that provides initial values for form fields. While superficially similar to
-- | Haskell's `Default` type class, this class is only meant for the purposes of defining
-- | initial form values. If you need a default value for a field in your form, you are better
-- | off defining it as a `Maybe` field and providing a default as part of validation.
-- |
-- | In general, if you find yourself reaching for this type class without defining your form
-- | as a row and generating your form spec from it, then you are probably not doing what you intend.
class Initial v where
  initial :: v

instance initialUnit :: Initial Unit where
  initial = mempty

instance initialBoolean :: Initial Boolean where
  initial = false

instance initialInt :: Initial Int where
  initial = 0

instance initialNumber :: Initial Number where
  initial = 0.0

instance initialString :: Initial String where
  initial = mempty

instance initialOrdering :: Initial Ordering where
  initial = mempty

instance initialMaybe :: Initial (Maybe a) where
  initial = Nothing

instance initialArray :: Initial (Array a) where
  initial = mempty

instance initialList :: Initial (List a) where
  initial = mempty

instance initialMap :: Ord k => Initial (Map k v) where
  initial = mempty

instance initialFn :: Monoid b => Initial (a -> b) where
  initial = mempty

instance initialTuple :: (Initial a, Initial b) => Initial (Tuple a b) where
  initial = Tuple initial initial

instance initialRecord :: (RL.RowToList row list, MonoidRecord list row row) => Initial (Record row) where
  initial = mempty

