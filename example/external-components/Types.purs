module Example.ExternalComponents.Types where

import Prelude

import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, User)
import Formless as F
import Ocelot.Components.Typeahead as TA

----------
-- Component

data Query a
  = HandleFormless (F.Message Query Form User) a
  | HandleTypeahead Slot (TA.Message Query String) a
  | Reset a

type State = Unit

type ChildQuery = F.Query Query FCQ FCS Form User Aff
type ChildSlot = Unit

-- | FCQ: Formless ChildQuery
-- | FCS: Formless ChildSlot
type FCQ = TA.Query Query String String Aff
type FCS = Slot

data Slot
  = EmailTypeahead
  | WhiskeyTypeahead
  | LanguageTypeahead
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
