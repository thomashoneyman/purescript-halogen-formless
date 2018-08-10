-- | Various helpers to make working with validation based on
-- | `Polyform.Validation` nicer when using Formless.
module Formless.Validation.Polyform where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Internal as Internal
import Formless.Spec (Validator(..))
import Polyform.Validation (Validation(..))
import Polyform.Validation as Validation
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

----------
-- Fields

-- | Turn a Polyform `Validation` validator into one that is compatible
-- | with the field-level validator expected by Formless
toValidator
  :: ∀ m e i o
   . Functor m
  => Validation m e i o
  -> Validator m e i o
toValidator (Validation f) =
  Validator $ (map <<< map) Validation.toEither f

----------
-- Full form

mkValidators
  :: ∀ m form fxs vs fs
   . Newtype (form Record (Validator m)) (Record vs)
  => RL.RowToList fs fxs
  => MakeValidators fxs fs vs
  => Record fs
  -> form Record (Validator m)
mkValidators r = wrap $ Internal.fromScratch builder
  where
    builder = mkValidatorsBuilder (RLProxy :: RLProxy fxs) r

-- | The class to efficiently wrap a record of newtypes
class MakeValidators (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkValidatorsBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance makeValidatorsNil :: MakeValidators RL.Nil row () where
  mkValidatorsBuilder _ _ = identity

instance makeValidatorsCons
  :: ( IsSymbol name
     , Functor m
     , Row.Cons name (Validation m e i o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidators tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidators (RL.Cons name (Validation m e i o) tail) row to where
  mkValidatorsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = toValidator $ Record.get _name r
      rest = mkValidatorsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

else instance makeValidatorsFail
  :: ( Fail
        ( Text "Failed to match instance. All members of the record must have the shape (Polyform.Validation m e i o) in order to be transformed into (Validator m e i o)."
        )
     , Row.Cons name (Validation m e i o) trash row
     , Newtype (form Record (Validator m)) (Record row)
     , MakeValidators tail row from
     , Internal.Row1Cons name (Validator m e i o) from to
     )
  => MakeValidators (RL.Cons name (Validation m e i o) tail) row to where
  mkValidatorsBuilder _ = unsafeCoerce

