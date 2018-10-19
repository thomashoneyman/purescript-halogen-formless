module Formless.Transform.Record where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Formless.Spec (InputField, OutputField)
import Heterogeneous.Mapping as HM

-- | Unwrap every newtype in a record filled with newtypes
data UnwrapField = UnwrapField

instance unwrapField :: (Newtype wrapper x) => HM.Mapping UnwrapField wrapper x where
  mapping UnwrapField = unwrap

unwrapRecord :: ∀ r0 r1. HM.HMap UnwrapField r0 r1 => r0 -> r1
unwrapRecord = HM.hmap UnwrapField

-- | Wrap every field in a record with a particular newtype
data WrapField = WrapField

instance wrapField :: (Newtype wrapper x) => HM.Mapping WrapField x wrapper where
  mapping WrapField = wrap

wrapRecord :: ∀ r0 r1. HM.HMap WrapField r0 r1 => r0 -> r1
wrapRecord = HM.hmap WrapField

unwrapOutputFields
  :: ∀ form os os'
   . Newtype (form Record OutputField) { | os }
  => HM.HMap UnwrapField { | os } { | os' }
  => form Record OutputField
  -> { | os' }
unwrapOutputFields = unwrapRecord <<< unwrap

wrapInputFields
  :: ∀ form is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> form Record InputField
wrapInputFields = wrap <<< wrapRecord

