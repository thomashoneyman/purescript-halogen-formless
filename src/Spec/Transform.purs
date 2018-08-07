module Formless.Spec.Transform where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormProxy, InputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy(..))

-- | A function to transform a row of labels into a InputFields. This allows you
-- | to go directly from a custom form newtype to a spec without having to
-- | fill in any values. Requires that all members have an instance of the
-- | `Initial` type class (all monoidal values do by default, along with some
-- | other primitives).
-- |
-- | ```purescript
-- | ```
mkInputFields
  :: ∀ xs form inputs
   . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeInputFieldsFromRow xs inputs inputs
  => FormProxy form
  -> form Record InputField
mkInputFields _ = wrap $ Internal.fromScratch builder
  where
    builder = mkInputFieldsFromRowBuilder
      (RLProxy :: RLProxy xs)
      (RProxy :: RProxy inputs)

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
   . RL.RowToList row xs
  => Newtype (form Record InputField) (Record row)
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

-- | Unwraps all the fields in a record, so long as all fields have newtypes
unwrapRecord
  :: ∀ row xs row'
   . RL.RowToList row xs
  => UnwrapRecord xs row row'
  => Record row
  -> Record row'
unwrapRecord = Internal.fromScratch <<< unwrapRecordBuilder (RLProxy :: RLProxy xs)

-- | Wraps all the fields in a record, so long as all fields have proper newtype
-- | instances
wrapRecord
  :: ∀ row xs row'
   . RL.RowToList row xs
  => WrapRecord xs row row'
  => Record row
  -> Record row'
wrapRecord = Internal.fromScratch <<< wrapRecordBuilder (RLProxy :: RLProxy xs)

-- | The class to efficiently unwrap a record of newtypes
class UnwrapRecord (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  unwrapRecordBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance unwrapRecordNil :: UnwrapRecord RL.Nil row () where
  unwrapRecordBuilder _ _ = identity

instance unwrapRecordCons
  :: ( IsSymbol name
     , Row.Cons name wrapper trash row
     , Newtype wrapper x
     , UnwrapRecord tail row from
     , Internal.Row1Cons name x from to
     )
  => UnwrapRecord (RL.Cons name wrapper tail) row to where
  unwrapRecordBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = unwrap $ Record.get _name r
      rest = unwrapRecordBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | The class to efficiently wrap a record of newtypes
class WrapRecord (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  wrapRecordBuilder :: RLProxy xs -> Record row -> Internal.FromScratch to

instance wrapRecordNil :: WrapRecord RL.Nil row () where
  wrapRecordBuilder _ _ = identity

instance wrapRecordCons
  :: ( IsSymbol name
     , Row.Cons name x trash row
     , Newtype wrapper x
     , WrapRecord tail row from
     , Internal.Row1Cons name wrapper from to
     )
  => WrapRecord (RL.Cons name x tail) row to where
  wrapRecordBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = wrap $ Record.get _name r
      rest = wrapRecordBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

