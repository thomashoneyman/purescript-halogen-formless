module FormlessTemplate where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Template.DataAndValidation (NAME_FIELD, _name, minLength)
import Type.Row (type (+))

type FormFieldsRow f =
  ( NAME_FIELD f
  + ()
  )

type FormFields = { | FormFieldsRow F.OutputType }

newtype Form r f = Form (r (FormFieldsRow f))
derive instance newtypeForm' :: Newtype (Form r f) _

-- Form component types

type Input = Unit
type AddedState = ()
data Action
  = DoStuff
  | Initialize
  | Finalize
  | Receive Input

data Query a
  = Command a
  | Reply (Unit -> a)

type Message = FormFields
type ChildSlots =
  ()
type MonadType = Aff
type SelfSlot index = F.Slot Form Query ChildSlots Message index

component :: F.Component Form Query ChildSlots Input FormFields Aff
component = F.component mkInput $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  , initialize = Just Initialize
  , finalize = Just Finalize
  , receive = Just <<< Receive
  }
  where
  mkInput :: Input -> F.Input Form AddedState Aff
  mkInput _ =
    { validators: Form
        { name: minLength 7
        }
    , initialInputs: Nothing
    }

  render :: F.PublicState Form AddedState -> F.ComponentHTML Form Action ChildSlots Aff
  render st =
    HH.div_
      [ HH.p_ [ HH.text $ "Validity: " <> show st.validity ]
      , HH.p_ [ HH.text $ "Dirty: " <> show st.dirty ]
      , HH.p_ [ HH.text $ "Being Submitted: " <> show st.submitting ]
      , HH.p_ [ HH.text $ "Number of Errors: " <> show st.errors ]
      , HH.p_ [ HH.text $ "Number of Submit attempts: " <> show st.submitAttempts ]
      -- not sure how to use this value: `st.form`
      , HH.div_
        [ HH.text "Label" ]
      , HH.input
        [ HP.type_ InputText
        , HP.placeholder "Michael"
        , HP.value (F.getInput _name st.form)
        , HE.onValueInput (Just <<< F.setValidate _name)
        ]
      , HH.button
        [ if st.submitting || st.validity /= F.Valid
            then HP.disabled true
            else HE.onClick \_ -> Just F.submit
        ]
        [ HH.text "Submit" ]
      ]

  handleEvent :: F.Event Form AddedState
              -> F.HalogenM Form AddedState Action ChildSlots Message MonadType Unit
  handleEvent = case _ of
    F.Submitted formContent -> do
      let sendFormDataToParent = H.raise $ F.unwrapOutputFields formContent
      -- but we don't call it in case you want a different message type
      pure unit
    F.Changed formState -> do
      void $ pure formState

  handleAction :: Action -> F.HalogenM Form AddedState Action ChildSlots Message MonadType Unit
  handleAction = case _ of
    DoStuff -> do
      pure unit
    Initialize -> do
      pure unit
    Receive input -> do
      pure unit
    Finalize -> do
      pure unit

  handleQuery :: forall a. Query a -> F.HalogenM Form AddedState Action ChildSlots Message MonadType (Maybe a)
  handleQuery = case _ of
    Reply reply -> do
      pure $ Just $ reply unit
    Command next -> do
      pure $ Just next
