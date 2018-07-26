module Example.ExternalComponents.Types where

import Prelude

import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, User)
import Formless as F
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component manages several typeahead components, plus
-- | F. Because of the external components, it needs its
-- | own reset query to clear those components when F
-- | has been reset by the user.
data Query a
  = HandleFormless (F.Message Query Form User) a
  | HandleTypeahead Slot (TA.Message Query String) a
  | Reset a

type State = Unit

-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = F.Query Query FCQ FCS Form User Aff
type ChildSlot = Unit


----------
-- F

-- | Your parent component must provide a ChildQuery type to F
-- | that represents what sorts of children it can have, and an accompanying
-- | child slot type. In this case we'll provide no child query or child slot.
-- |
-- | FCQ: F ChildQuery
-- | FCS: F ChildSlot
type FCQ = TA.Query Query String String Aff
type FCS = Slot

data Slot
  = EmailTypeahead
  | WhiskeyTypeahead
  | LanguageTypeahead

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
