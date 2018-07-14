module Example.RealWorld.Types where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (Admin, GroupForm)
import Example.RealWorld.Data.Options (Metric, OptionsForm)
import Formless as Formless
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleGroupForm (Formless.Message Query GroupForm) a
  | HandleOptionsForm (Formless.Message Query OptionsForm) a
  | HandleGroupTypeahead GroupTASlot (TA.Message Query String) a
  | HandleAdminDropdown (Dropdown.Message Admin) a
  | HandleMetricDropdown (Dropdown.Message Metric) a
  | Select Tab a
	| Submit a

type State =
  { focus :: Tab }

-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = Coproduct2
  (Formless.Query Query GroupCQ GroupCS GroupForm Aff)
  (Formless.Query Query OptionsCQ OptionsCS OptionsForm Aff)

type ChildSlot = Either2
  Unit
  Unit

----------
-- Formless

-- | Types for the group form
type GroupCQ = Coproduct2
  (TA.Query Query String String Aff)
  (Dropdown.Query Admin)

type GroupCS = Either2
  GroupTASlot
  Unit

-- | Types for the options form
type OptionsCQ = Dropdown.Query Metric
type OptionsCS = Unit

----------
-- Slots

data GroupTASlot
  = ApplicationsTypeahead
  | PixelsTypeahead
  | WhiskeyTypeahead
derive instance eqGroupTASlot :: Eq GroupTASlot
derive instance ordGroupTASlot :: Ord GroupTASlot

----------
-- Navigation

data Tab
  = GroupFormTab
  | OptionsFormTab
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab
