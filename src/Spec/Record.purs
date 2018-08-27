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

fromForm_ :: forall sym. GetProp sym -> FormFieldGetFields sym
fromForm_ g = hmap g <<< unwrapRecord <<< unwrap

-- | Given a form, get a record of all the fields with their input values 
getInputs :: FormFieldGetFields "input"
getInputs = fromForm_ (GetProp (SProxy :: SProxy "input"))

-- | Given a form, get a record of all the fields with their touched status
getToucheds :: FormFieldGetFields "touched"
getToucheds = fromForm_ (GetProp (SProxy :: SProxy "touched"))

-- | Given a form, get a record of all the fields with their result values
getResults :: FormFieldGetFields "result"
getResults = fromForm_ (GetProp (SProxy :: SProxy "result"))
