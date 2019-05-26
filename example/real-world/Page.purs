module Example.RealWorld.Page where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Element as UI
import Example.RealWorld.GroupForm as GF
import Halogen as H
import Halogen.HTML as HH

-- Despite being a complex form, all the page component needs to think about
-- is the correct output type.
data Action
  = HandleGroupForm GF.Group

type State =
  { group :: Maybe GF.Group }

type ChildSlots =
  ( groupForm :: GF.Slot Unit )

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  initialState :: State
  initialState = { group: Nothing }

  handleAction = case _ of
    HandleGroupForm group -> do
      H.modify_ _ { group = Just group }
      H.liftEffect $ Console.logShow group

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render st =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A complex form inspired by real-world use cases." ]
      , UI.p_
          """
          This component demonstrates building a large form with complex rendering and validation requirements. Notice how both tabs end up unifying to a single output type after the two forms are combined, how various dropdowns determine the contents (and visibility) of other form elements, the assorted external components, and how validation for many fields depends on the values of other fields in the form.
          """
      , HH.br_
      , UI.p_
          """
          Next, review the source code. You'll notice that all of the complex types and state necessary to run this form can be generated from a pair of row types. All that's left for you to handle is to write the validation (with helper functions) and the render function.
          """
      , HH.br_
      , HH.slot GF._groupForm unit GF.component unit handler
      ]
    where
    handler = Just <<< HandleGroupForm
