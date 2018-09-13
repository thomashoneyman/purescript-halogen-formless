module Example.RealWorld.Types where

import Prelude

import Data.Either.Nested (Either2, Either3)
import Data.Functor.Coproduct.Nested (Coproduct2, Coproduct3)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Example.App.UI.Dropdown as Dropdown
import Example.App.UI.Typeahead as TA
import Example.RealWorld.Data.Group (Admin, Group, GroupForm)
import Example.RealWorld.Data.Options (Metric, OptionsForm)
import Formless as Formless

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = GroupForm (Formless.Message Query GroupForm) a
  | OptionsForm (Formless.Message Query OptionsForm) a
  | TASingle (TA.Message Maybe String) a
  | TAMulti GroupTASlot (TA.Message Array String) a
  | AdminDropdown (Dropdown.Message Admin) a
  | MetricDropdown (Dropdown.Message Metric) a
  | Select Tab a
  | Reset a
  | Submit a

-- | We'll keep track of both form errors so we can show them in tabs
-- | and our ultimate goal is to result in a Group we can send to the
-- | server.
type State =
  { focus :: Tab                 -- Which tab is the user on?
  , groupFormErrors :: Int       -- Count of the group form errors
  , groupFormDirty :: Boolean    -- Is the group form in a dirty state?
  , optionsFormErrors :: Int     -- Count of the options form errors
  , optionsFormDirty :: Boolean  -- Is the options form in a dirty state?
  , optionsEnabled :: Boolean    -- Is the options form enabled?
  , group :: Maybe Group         -- Our ideal result type from form submission
  }

-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = Coproduct2
  (Formless.Query Query GroupCQ GroupCS GroupForm Aff)
  (Formless.Query Query (Dropdown.Query Metric) Unit OptionsForm Aff)

type ChildSlot = Either2 Unit Unit

----------
-- Formless

-- | Types for the group form
type GroupCQ = Coproduct3 (TA.Query String) (TA.Query String) (Dropdown.Query Admin)
type GroupCS = Either3 GroupTASlot Unit Unit

----------
-- Slots

data GroupTASlot = Applications | Pixels
derive instance eqGroupTASlot :: Eq GroupTASlot
derive instance ordGroupTASlot :: Ord GroupTASlot

----------
-- Navigation

data Tab = GroupTab | OptionsTab
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab
