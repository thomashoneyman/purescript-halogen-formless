module Example.CustomBehavior.Types where

import Prelude

import Effect.Aff (Aff)
import Example.CustomBehavior.Spec (Form)
import Formless as Formless
import Ocelot.Components.Dropdown as Dropdown

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleFormless (Formless.Message Query Form) a
  | HandleColorDropdown (Dropdown.Message String) a
  | HandleObjectDropdown (Dropdown.Message String) a

type State = Unit

-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = Formless.Query Query FCQ FCS Form Aff
type ChildSlot = Unit

----------
-- Formless

-- | Your parent component must provide a ChildQuery type to Formless
-- | that represents what sorts of children it can have, and an accompanying
-- | child slot type. In this case we'll provide no child query or child slot.
-- |
-- | FCQ: Formless ChildQuery
-- | FCS: Formless ChildSlot
type FCQ = Dropdown.Query String
type FCS = Int


