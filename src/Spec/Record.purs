module Formless.Spec.Record where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Formless.Spec (FormField)
import Formless.Spec.Transform (class UnwrapRecord, unwrapRecord)
import Heterogeneous.Mapping (class HMap, class MapRecordWithIndex, class Mapping, ConstMapping, hmap)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, SProxy(..))

newtype GetProp (prop :: Symbol) = GetProp (SProxy prop)

instance getProp ::
  (IsSymbol prop, Row.Cons prop a rx r) =>
  Mapping (GetProp prop) { | r } a where
  mapping (GetProp prop) = Record.get prop


type FormFieldGetFields sym =
  forall a xs form fields rout ys
   . Newtype (form Record FormField) { | fields }
  => RL.RowToList fields xs
  => UnwrapRecord xs fields a
  => RL.RowToList a ys
  => HMap (GetProp sym) { | a } { | rout }
  => MapRecordWithIndex ys (ConstMapping (GetProp sym)) a rout
  => form Record FormField 
  -> { | rout }

-- | Given a form, get a record containing all the inputs
getInputs :: FormFieldGetFields "input"
getInputs = hmap (GetProp (SProxy :: SProxy "input")) <<< unwrapRecord <<< unwrap
