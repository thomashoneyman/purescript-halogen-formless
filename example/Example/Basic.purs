-- | This example shows a bare-bones simple form with validation, suitable for
-- | typical small forms in an application.
module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Example.Field.Basic.Text (textField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (buildForm, initialFormState, useForm)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)
import Web.HTML as HTML
import Web.HTML.Window as Window

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useForm (\_ -> initialFormState) $ buildForm
    { name: textField proxy
        { validate: note "Name is required." <<< NES.fromString }
    , message: textField proxy
        { validate: pure }
    }

  Hooks.pure do
    HH.form
      [ HE.onSubmit \event -> Just $ liftEffect do
          preventDefault event
          HTML.window >>= Window.alert (show form.value)
      ]
      [ form.fields.name.input
      , form.fields.message.input
      , HH.button
          [ HP.type_ HP.ButtonSubmit
          , HP.disabled (isNothing form.value || not form.touched)
          ]
          [ HH.text "Submit!" ]
      ]
  where
  -- We are using the compiler to infer our form type. When doing this, we need
  -- to provide a proxy for our monad type, `m`, to each of our form inputs. This
  -- aids the compiler in type inference.
  proxy = Proxy2 :: Proxy2 m
