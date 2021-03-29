-- | This example form uses an explicit type for its form fields. This can help
-- | with type inference -- when using an explicit type you do not need to provide
-- | a proxy for the `m` type parameter -- but most users prefer to let the
-- | compiler infer types for their forms.
module Example.BasicAnnotated where

import Prelude

import Data.Either (note)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Example.Field.Basic.Text (UseBasicTextField, BasicTextFieldInterface, textField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormField(..), buildForm, initialFormState, useForm)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)
import Web.HTML as HTML
import Web.HTML.Window as Window

type Form f m =
  { name :: f m (UseBasicTextField NonEmptyString) (BasicTextFieldInterface m) String NonEmptyString
  , message :: f m Hooks.Pure (input :: HH.ComponentHTML (HookM m Unit) () m) String String
  }

fields :: forall m. Form FormField m
fields =
  { name: textField Proxy2 { validate: note "Name is required." <<< NES.fromString }
  , message: FormField Proxy2 \field -> Hooks.pure
      { input:
          HH.div_
            [ HH.input
                [ HP.value (fromMaybe "" field.value)
                , HE.onValueInput field.onChange
                ]
            ]
      , value: field.value
      }
  }

basic :: forall q i o m. MonadEffect m => H.Component q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useForm (\_ -> initialFormState) (buildForm fields)

  Hooks.pure do
    HH.form
      [ HE.onSubmit \ev -> liftEffect do
          preventDefault ev
          HTML.window >>= Window.alert (show form.value)
      ]
      [ form.fields.name.input
      , form.fields.message.input
      , HH.button
          [ HP.disabled (isNothing form.value || not form.touched)
          , HP.type_ HP.ButtonSubmit
          ]
          [ HH.text "Submit!" ]
      ]
