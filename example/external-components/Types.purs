module Example.ExternalComponents.Types where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Example.App.UI.Typeahead as TA
import Example.ExternalComponents.Spec (Form)
import Formless as F

----------
-- Component

data Query a
  = Formless (F.Message Query Form) a
  | Typeahead Slot (TA.Message Maybe String) a
  | Reset a

type State = Unit

type ChildQuery = F.Query Query (TA.Query String) Slot Form Aff
type ChildSlot = Unit

data Slot
  = Email
  | Whiskey
  | Language
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
