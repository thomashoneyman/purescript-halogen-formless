module Example.ExternalComponents.Types where

import Prelude

import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form)
import Formless as Formless
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleFormless (Formless.Message Query) a
  | HandleTypeahead (TA.Message Query String) a

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
type FCQ = TA.Query Query String String Aff
type FCS = Unit



