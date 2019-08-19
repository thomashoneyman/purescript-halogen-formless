module Example.Nested.Page where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Element as UI
import Example.Nested.Form as Form
import Halogen as H
import Halogen.HTML as HH

data Query a
  = HandleEventForm Form.Event

type ChildSlots =
  ( eventForm :: Form.Slot Unit )

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render st =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A form with a dynamic array of nested sub-forms." ]
      , UI.p_
          """
          It is possible to nest sub-forms within Formless like any other external component. This allows you to create an arbitrary number of forms within other forms while preserving the type safety provided by the Formless library. Try submitting the form with no sub-forms, and review the output in the console. Next, try adding one or more sub-forms and submitting them when valid or invalid.
          """
      , HH.br_
      , HH.slot _event unit Form.eventComponent unit handler
      ]
    where
    _event = SProxy :: _ "eventForm"
    handler = Just <<< HandleEventForm

  handleAction = case _ of
    HandleEventForm event -> H.liftEffect $ Console.logShow event
