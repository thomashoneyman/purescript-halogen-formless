-- | This example form uses an explicit type for its form fields. This can help
-- | with type inference -- when using an explicit type you do not need to provide
-- | a proxy for the `m` type parameter -- but most users prefer to let the
-- | compiler infer types for their forms.
module Example.BasicAnnotated where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Example.Field.Basic (BasicFieldInterface, UseBasicField, basicField')
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormField, buildForm, initialFormState, useForm)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

type Form m =
  { name :: FormField m (UseBasicField NonEmptyString) (BasicFieldInterface m) String NonEmptyString
  , message :: FormField m (UseBasicField String) (BasicFieldInterface m) String String
  }

formFields :: forall m. Form m
formFields =
  { name: basicField'
      { validate: note "Name is required." <<< NES.fromString
      , initialValue: Just "Tom"
      }
  , message: basicField'
      { validate: pure
      , initialValue: Nothing
      }
  }

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useForm (\_ -> initialFormState) (buildForm formFields)

  Hooks.pure do
    HH.div_
      [ form.fields.name.input
      , form.fields.message.input
      , HH.button
          [ HP.disabled (isNothing form.value || not form.touched)
          , HE.onClick \e -> Just do
              liftEffect (preventDefault (ME.toEvent e))
              logShow form.value
          ]
          [ HH.text "Submit!" ]
      ]
