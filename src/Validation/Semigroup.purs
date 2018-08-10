-- | Various helpers to make working with validation based on
-- | `Data.Validation` nicer when using Formless.
module Formless.Validation.Semigroup where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Validation.Semigroup (V, toEither)
import Formless.Internal as Internal
import Formless.Spec (Validator(..), hoistFn, hoistFnE)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

----------
-- Field level

-- | Turn a semigroup validator into one that is compatible
-- | with the field-level validator expected by Formless
toValidator
  :: ∀ m e i o
   . Monad m
  => (i -> V e o)
  -> Validator m e i o
toValidator = hoistFnE <<< map toEither

-- | Turn a monadic semigroup validator into one that is compatible
-- | with the field-level validator expected by Formless
toValidatorM
  :: ∀ m e i o
   . Monad m
  => (i -> m (V e o))
  -> Validator m e i o
toValidatorM = Validator <<< (map <<< map) toEither

----------
-- Full form

-- | When you have a record where all members are (i -> o) you can turn them
-- | all into proper Validators with this function.
mkValidatorsFn
  :: ∀ m vs fxs form fs
   . Newtype (form Record (Validator m)) (Record vs)
  => RL.RowToList fs fxs
  => MakeValidatorsFn fxs fs vs
  => Record fs
  -> form Record (Validator m)
mkValidatorsFn r = wrap $ Internal.fromScratch builder
  where
    builder = mkValidatorsFnBuilder (RLProxy :: RLProxy fxs) r

-- | The class to efficiently wrap a record of newtypes
class MakeValidatorsFn (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkValidatorsFnBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance makeValidatorsFnNil :: MakeValidatorsFn RL.Nil row () where
  mkValidatorsFnBuilder _ _ = identity

instance makeValidatorsFnCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (i -> o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidatorsFn tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidatorsFn (RL.Cons name (i -> o) tail) row to where
  mkValidatorsFnBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = hoistFn $ Record.get _name r
      rest = mkValidatorsFnBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

else instance makeValidatorsFnFail
  :: ( Fail (Text "Could not match MakeValidatorsFN instance. All members of the record must have the shape (i -> o) in order to become (Validator m e i o).")
     , IsSymbol name
     , Monad m
     , Row.Cons name (i -> o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidatorsFn tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidatorsFn (RL.Cons name (i -> o) tail) row to where
  mkValidatorsFnBuilder _ = unsafeCoerce

-- | When you have a record where all members are (i -> V e o) you can turn them
-- | all into proper Validators with this function.
mkValidatorsFnV
  :: ∀ m vs vxs form fs
   . Newtype (form Record (Validator m)) (Record vs)
  => RL.RowToList vs vxs
  => MakeValidatorsFnV vxs fs vs
  => Record fs
  -> form Record (Validator m)
mkValidatorsFnV r = wrap $ Internal.fromScratch builder
  where
    builder = mkValidatorsFnVBuilder (RLProxy :: RLProxy vxs) r

-- | The class to efficiently wrap a record of newtypes
class MakeValidatorsFnV (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkValidatorsFnVBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance makeValidatorsFnVNil :: MakeValidatorsFnV RL.Nil row () where
  mkValidatorsFnVBuilder _ _ = identity

instance makeValidatorsFnVCons
  :: ( IsSymbol name
     , Monad m
     , Row.Cons name (i -> V e o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidatorsFnV tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidatorsFnV (RL.Cons name (i -> V e o) tail) row to where
  mkValidatorsFnVBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = toValidator $ Record.get _name r
      rest = mkValidatorsFnVBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

else instance makeValidatorsFnVFail
  :: ( Fail (Text "Could not match MakeValidatorsFnV instance. All members of the record must have the shape (i -> V e o) in order to become (Validator m e i o).")
     , IsSymbol name
     , Monad m
     , Row.Cons name (i -> V e o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidatorsFnV tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidatorsFnV (RL.Cons name (i -> V e o) tail) row to where
  mkValidatorsFnVBuilder _ = unsafeCoerce
