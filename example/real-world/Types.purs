module Example.RealWorld.Types where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (GroupForm)
import Example.RealWorld.Data.Options (OptionsForm)
import Formless as Formless
import Ocelot.Components.Typeahead as TA

----------
-- Component

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleGroupForm (Formless.Message Query GroupForm) a
  | HandleOptionsForm (Formless.Message Query OptionsForm) a
  | HandleTypeahead TypeaheadSlot (TA.Message Query String) a

type State = Unit

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
type GroupCQ = TA.Query Query String String Aff
type GroupCS = TypeaheadSlot

-- | Types for the options form
type OptionsCQ = TA.Query Query String String Aff
type OptionsCS = TypeaheadSlot

----------
-- Slots

data TypeaheadSlot
  = EmailTypeahead
  | WhiskeyTypeahead
  | LanguageTypeahead

derive instance eqTypeaheadSlot :: Eq TypeaheadSlot
derive instance ordTypeaheadSlot :: Ord TypeaheadSlot
