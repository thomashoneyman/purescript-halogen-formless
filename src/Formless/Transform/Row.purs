module Formless.Transform.Row where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal.Transform (class Row1Cons, FromScratch, fromScratch)
import Formless.Types.Form (InputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

----------
-- Construction from rows

-- | A function to transform a row of labels into a InputFields. This allows you
-- | to go directly from a custom form newtype to an inputs record without having to
-- | fill in any values. Requires that all members have an instance of the
-- | `Initial` type class (all monoidal values do by default, along with some
-- | other primitives).
mkInputFields
  :: forall xs form inputs
   . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeInputFieldsFromRow xs inputs inputs
  => form Record InputField
mkInputFields = wrap $ fromScratch builder
  where
  builder = mkInputFieldsFromRowBuilder
    (Proxy :: Proxy xs)
    (Proxy :: Proxy inputs)

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper InputFields by wrapping it in newtypes and
-- | supplying initial values
class MakeInputFieldsFromRow (xs :: RL.RowList Type) (row :: Row Type) (to :: Row Type) | xs -> to where
  mkInputFieldsFromRowBuilder :: Proxy xs -> Proxy row -> FromScratch to

instance mkInputFieldsFromRowNil :: MakeInputFieldsFromRow RL.Nil row () where
  mkInputFieldsFromRowBuilder _ _ = identity

instance mkInputFieldsFromRowCons ::
  ( IsSymbol name
  , Initial i
  , Row.Cons name (InputField e i o) trash row
  , MakeInputFieldsFromRow tail row from
  , Row1Cons name (InputField e i o) from to
  ) =>
  MakeInputFieldsFromRow (RL.Cons name (InputField e i o) tail) row to where
  mkInputFieldsFromRowBuilder _ r =
    first <<< rest
    where
    _name = Proxy :: Proxy name
    val = InputField initial
    rest = mkInputFieldsFromRowBuilder (Proxy :: Proxy tail) r
    first = Builder.insert _name val

-- | A type to collect constraints necessary to apply to prove that a record of
-- | SProxies is compatible with your form type.
type SProxies form =
  forall xs row inputs
   . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeSProxies xs row
  => Record row

-- | A helper function to produce a record of SProxies given a form spec, to save
-- | you the boilerplate of writing them all out.
mkSProxies
  :: forall form xs inputs row
   . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeSProxies xs row
  => Proxy form
  -> Record row
mkSProxies _ = fromScratch builder
  where
  builder = makeSProxiesBuilder (Proxy :: Proxy xs)

-- | The class used to build up a new record of symbol proxies from an
-- | input row list.
class MakeSProxies (xs :: RL.RowList Type) (to :: Row Type) | xs -> to where
  makeSProxiesBuilder :: Proxy xs -> FromScratch to

instance makeSProxiesNil :: MakeSProxies RL.Nil () where
  makeSProxiesBuilder _ = identity

instance makeSProxiesCons ::
  ( IsSymbol name
  , Row1Cons name (Proxy name) from to
  , MakeSProxies tail from
  ) =>
  MakeSProxies (RL.Cons name x tail) to where
  makeSProxiesBuilder _ = first <<< rest
    where
    rest = makeSProxiesBuilder (Proxy :: Proxy tail)
    first = Builder.insert (Proxy :: Proxy name) (Proxy :: Proxy name)
