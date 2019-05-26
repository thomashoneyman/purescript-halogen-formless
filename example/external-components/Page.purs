module Example.ExternalComponents.Page where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Example.App.UI.Element as UI
import Example.ExternalComponents.Form (ChildSlots, User, UserForm)
import Example.ExternalComponents.Form as Form
import Formless as F
import Halogen as H
import Halogen.HTML as HH

data Action
  = HandleFormless User

type ChildSlot =
  ( formless :: F.Slot UserForm (Const Void) ChildSlots User Unit )

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleFormless user -> logShow (user :: User)

  render st =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A form leveraging external components and custom form actions." ]
      , UI.p_
          """
          In Formless, you can freely leverage external components and embed them in the form. This form shows how to use custom typeahead components built with Select from CitizenNet. This form also demonstrates how you can manipulate forms in Formless. Try selecting an email address, then a whiskey. You'll notice that changing your whiskey selection also clears the selected email.

          Next, try opening the console. If you submit the form with invalid values, Formless will show you your errors. If you submit a valid form, you'll see Formless just returns the valid outputs for you to work with.

          """
      , HH.slot F._formless unit Form.component unit (Just <<< HandleFormless)
      ]
