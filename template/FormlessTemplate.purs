module FormlessTemplate where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Formless (useFormless)
import Formless as F
import Formless.UseFormless (FormlessEvent(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
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
component
  :: forall m query input message
   . MonadAff m
  => H.Component HH.HTML query input message m
component = Hooks.component \_ _ -> Hooks.do
  additionalState /\ additionalStateId <- useState (Just 5)
  event <- useEvent
  formless <- useFormless
    { validators: Form
        { name: minLength 7
        }
    , initialInputs: Nothing -- when Nothing, will use `Initial` type class
    , pushChange: event.push <<< Changed
    , pushSubmitted: event.push <<< Submitted
    }

  Hooks.useLifecycleEffect do
    -- Decide what, if anything, to do when Formless events occur.
    -- For example, if you would like to raise events as messages,
    -- then use `F.raiseResult` as your `handleEvent` function.
    _ <- event.setCallback $ Just \_ e -> case e of
        -- Indicates that the form has been successfully submitted.
        Submitted formContent -> do
          -- This is how to get the output values of the form.
          let formFields = F.unwrapOutputFields formContent

          -- We won't do this here, but this is how most will handle a form
          -- submission: raise it as an event to their parent.
          --      H.raise formFields

          -- Alternatively, one could do something custom with the output values.

          -- This line exists so the code compiles.
          pure unit

        -- Indicates that the form's content has been changed.
        -- This event is triggered anytime a field is changed,
        -- whether it passes validation or not.
        Changed formState -> do
          void $ pure formState

    pure Nothing

  Hooks.pure $
    HH.div_
      -- Indicates whether the form's values are valid
      -- (i.e. validation has passed for all fields)
      [ HH.p_ [ HH.text $ "Validity: " <> show formless.state.validity ]

      -- Indicates whether any field in the form has been changed from
      -- its initial state
      , HH.p_ [ HH.text $ "Dirty: " <> show formless.state.dirty ]

      -- Indicates whether the 'submit' button has been clicked and the
      -- form's content is still being validated one last time before
      -- submission is accepted.
      , HH.p_ [ HH.text $ "Being Submitted: " <> show formless.state.submitting ]

      -- Indicates the number of errors due to validation failing that
      -- need to be fixed before submission is accepted
      , HH.p_ [ HH.text $ "Number of Errors: " <> show formless.state.errors ]

      -- Indicates the number of times user has attempted to submit the form
      , HH.p_ [ HH.text $ "Number of Submit attempts: " <> show formless.state.submitAttempts ]

      -- We can also refer to any additional labels we used to extend the
      -- form's state; in this case, that means any field from our
      -- `AddedState` type.
      , HH.p_ [ HH.text $ "Additional state was: " <> show additionalState ]

      , HH.div_
        [ HH.text "Label" ]
      , HH.input
        [ HP.type_ InputText
        , HP.placeholder "Michael"

        -- gets the value of `_name` in the form's state
        , HP.value (F.getInput _name formless.state.form)

        -- sets the value of `_name` and then validates it
        , HE.onValueInput (Just <<< F.setValidate formless _name)
        ]
      , HH.button
        [ if formless.state.submitting || formless.state.validity /= F.Valid
            then HP.disabled true
            else HE.onClick \_ -> Just formless.submit
        ]
        [ HH.text "Submit" ]
      ]
