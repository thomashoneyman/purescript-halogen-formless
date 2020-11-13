module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Example.Input.Basic (basicInput)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (buildForm, initialFormState, useFormWithState)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useFormWithState (\_ -> initialFormState) $ buildForm
    { name: basicInput proxy
        { validate: note "Name is required." <<< NES.fromString
        , initialValue: Just "Tom"
        }
    , location: basicInput proxy
        { validate: pure
        , initialValue: Nothing
        }
    }

  Hooks.pure do
    HH.div_
      [ form.fields.name.input
      , form.fields.location.input
      , HH.button
          [ HP.disabled (isNothing form.value || not form.dirty)
          , HE.onClick \e -> Just do
              liftEffect (preventDefault (ME.toEvent e))
              logShow form.value
          ]
          [ HH.text "Submit!" ]
      ]
  where
  proxy :: Proxy2 m
  proxy = Proxy2
