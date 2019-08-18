module FormlessTemplate where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
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
type AddedState = ( additionalState :: Maybe Int )
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
type SelfSlot index = F.Slot Form Query ChildSlots Message index

component
  :: forall m
   . MonadAff m
  => F.Component Form Query ChildSlots Input FormFields m
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
  -- Converts the Input value passed in by the parent component
  -- into the Formless' Input value.
  mkInput :: Input -> F.Input Form AddedState m
  mkInput _ =
    -- the two values here are for Formless
    { validators: Form
        { name: minLength 7
        }
    , initialInputs: Nothing -- when Nothing, will use `Initial` type class

    -- everything else below comes from our `AddedState` rows:
    , additionalState: Just 5
    }

  render
    :: F.PublicState Form AddedState
    -> F.ComponentHTML Form Action ChildSlots m
  render st =
    HH.div_
      [ HH.p_ [ HH.text $ "Validity: " <> show st.validity ]
      , HH.p_ [ HH.text $ "Dirty: " <> show st.dirty ]
      , HH.p_ [ HH.text $ "Being Submitted: " <> show st.submitting ]
      , HH.p_ [ HH.text $ "Number of Errors: " <> show st.errors ]
      , HH.p_ [ HH.text $ "Number of Submit attempts: " <> show st.submitAttempts ]
      , HH.p_ [ HH.text $ "Additional state was: " <> show st.additionalState ]

      , HH.div_
        [ HH.text "Label" ]
      , HH.input
        [ HP.type_ InputText
        , HP.placeholder "Michael"
        , HP.value (F.getInput _name st.form) -- access one value in form
        , HE.onValueInput (Just <<< F.setValidate _name)
        ]
      , HH.button
        [ if st.submitting || st.validity /= F.Valid
            then HP.disabled true
            else HE.onClick \_ -> Just F.submit
        ]
        [ HH.text "Submit" ]
      ]

  -- Decide what, if anything, to do when Formless events occur.
  -- For example, if you would like to raise events as messages,
  -- then use `F.raiseResult` as your `handleEvent` function.
  handleEvent
    :: F.Event Form AddedState
    -> F.HalogenM Form AddedState Action ChildSlots Message m Unit
  handleEvent = case _ of
    F.Submitted formContent -> do
      -- This is how to get the output values of the form.
      let formFields = F.unwrapOutputFields formContent

      -- We won't do this here, but this is how most will handle a form
      -- submission: raise it as an event to their parent.
      --      H.raise formFields

      -- Alternatively, one could do something custom with the output values.

      -- This line exists so the code compiles.
      pure unit
    F.Changed formState -> do
      void $ pure formState

  handleAction
    :: Action
    -> F.HalogenM Form AddedState Action ChildSlots Message m Unit
  handleAction = case _ of
    DoStuff -> do
      pure unit
    Initialize -> do
      pure unit
    Receive input -> do
      pure unit
    Finalize -> do
      pure unit

  handleQuery
    :: forall a
     . Query a
    -> F.HalogenM Form AddedState Action ChildSlots Message m (Maybe a)
  handleQuery = case _ of
    Reply reply -> do
      pure $ Just $ reply unit
    Command next -> do
      pure $ Just next
