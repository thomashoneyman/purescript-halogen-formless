module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Example.Field.Basic (basicField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (buildForm, initialFormState, useFormFields, useFormState)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

-- type Form f m =
--   { name :: f m (UseBasicInput NonEmptyString) (BasicInputInterface m) String NonEmptyString
--   , location :: f m (UseBasicInput String) (BasicInputInterface m) String String
--   }

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useBasicForm (Proxy2 :: Proxy2 m)

  Hooks.pure do
    HH.div_
      [ form.fields.name.input
      , form.fields.location.input
      , HH.button
          [ HP.disabled (isNothing form.value || not form.touched)
          , HE.onClick \e -> Just do
              liftEffect (preventDefault (ME.toEvent e))
              logShow form.value
          ]
          [ HH.text "Submit!" ]
      ]
  where
  useBasicForm proxy = Hooks.do
    state <- useFormState (\_ -> initialFormState)
    useFormFields state $ buildForm
      { name: basicField
          { validate: note "Name is required." <<< NES.fromString
          , initialValue: Just "Tom"
          , proxy
          }
      , location: basicField
          { validate: pure
          , initialValue: Nothing
          , proxy
          }
      }

  -- useForm :: Hooks.Hook m (UseFormWithState (Form ToFormState m) m (UseBuildForm (Form ToFormHooks m))) (UseFormResult (Form ToFormState m) m (Form ToFormField m) (Form ToFormOutput m))
  -- useForm = useFormWithState (\_ -> initialFormState) (buildForm formInputs)

  -- built :: BuildFormInput (Form ToFormState m) (Form ToFormState m) m (UseBuildForm (Form ToFormHooks m)) (Form ToFormField m) (Form ToFormOutput m)
  -- built = buildForm rawForm
