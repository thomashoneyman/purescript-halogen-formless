module Formless.Spec.Transform where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormProxy, InputField(..), OutputField, Validator)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy(..))

-- | A function to unwrap a record of successful results into an equivalent
-- | record without any newtypes.
-- |
-- | ```purescript
-- | ```
unwrapOutput
  :: ∀ row xs form
   . RL.RowToList (form OutputField) xs
  => Internal.UnwrapRecord xs (form OutputField) row
  => Record (form OutputField)
  -> Record row
unwrapOutput = Internal.unwrapRecord

-- | A function to transform a record of validators into correct one for the form
-- |
-- | ```purescript
-- | ```

mkValidators
  :: ∀ row xs form m
   . RL.RowToList row xs
  => Internal.WrapRecord xs row (form (Validator m))
  => Record row
  -> Record (form (Validator m))
mkValidators = Internal.wrapRecord

-- | A function to transform a record of inputs of labels into a InputFields.
-- |
-- | ```purescript
-- | ```
mkInputFields
  :: ∀ row xs form
   . RL.RowToList row xs
  => Internal.WrapRecord xs row (form InputField)
  => Record row
  -> Record (form InputField)
mkInputFields = Internal.wrapRecord

-- | A function to transform a row of labels into a InputFields. This allows you
-- | to go directly from a custom form newtype to a spec without having to
-- | fill in any values. Requires that all members have an instance of the
-- | `Initial` type class (all monoidal values do by default, along with some
-- | other primitives).
-- |
-- | ```purescript
-- | ```
mkInputFieldsFromProxy
  :: ∀ xs form
   . RL.RowToList (form InputField) xs
  => MakeInputFieldsFromRow xs (form InputField) (form InputField)
  => FormProxy form
  -> Record (form InputField)
mkInputFieldsFromProxy _ = Internal.fromScratch builder
  where
    builder = mkInputFieldsFromRowBuilder
      (RLProxy :: RLProxy xs)
      (RProxy :: RProxy (form InputField))

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper InputFields by wrapping it in newtypes and
-- | supplying initial values
class MakeInputFieldsFromRow (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkInputFieldsFromRowBuilder :: RLProxy xs -> RProxy row -> Internal.FromScratch to

instance mkInputFieldsFromRowNil :: MakeInputFieldsFromRow RL.Nil row () where
  mkInputFieldsFromRowBuilder _ _ = identity

instance mkInputFieldsFromRowCons
  :: ( IsSymbol name
     , Initial i
     , Row.Cons name (InputField e i o) trash row
     , MakeInputFieldsFromRow tail row from
     , Internal.Row1Cons name (InputField e i o) from to
     )
  => MakeInputFieldsFromRow (RL.Cons name (InputField e i o) tail) row to where
  mkInputFieldsFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = InputField initial
      rest = mkInputFieldsFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | A type to collect constraints necessary to apply to prove that a record of
-- | SProxies is compatible with your form type.
type SProxies form =
   ∀ form xs row
    . RL.RowToList (form InputField) xs
   => MakeSProxies xs row
   => Record row

-- | A helper function to produce a record of SProxies given a form spec, to save
-- | you the boilerplate of writing them all out.
-- |
-- | ```purescript
-- | ```
mkSProxies
  :: ∀ form xs row
   . RL.RowToList (form InputField) xs
  => MakeSProxies xs row
  => FormProxy form
  -> Record row
mkSProxies _ = Internal.fromScratch builder
  where
    builder = makeSProxiesBuilder (RLProxy :: RLProxy xs)

-- | The class used to build up a new record of symbol proxies from an
-- | input row list.
class MakeSProxies (xs :: RL.RowList) (to :: # Type) | xs -> to where
  makeSProxiesBuilder :: RLProxy xs -> Internal.FromScratch to

instance makeSProxiesNil :: MakeSProxies RL.Nil () where
  makeSProxiesBuilder _ = identity

instance makeSProxiesCons
  :: ( IsSymbol name
     , Internal.Row1Cons name (SProxy name) from to
     , MakeSProxies tail from
     )
  => MakeSProxies (RL.Cons name x tail) to where
  makeSProxiesBuilder _ = first <<< rest
    where
      rest = makeSProxiesBuilder (RLProxy :: RLProxy tail)
      first = Builder.insert (SProxy :: SProxy name) (SProxy :: SProxy name)

