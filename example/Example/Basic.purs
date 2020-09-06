module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.NonEmpty as NES
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Example.Input.Basic (basicInput)
import Halogen (RefLabel(..))
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
basic = Hooks.component \_ (_ :: i) -> Hooks.do
  form <- useFormWithState (\_ -> initialFormState) $ buildForm
    { name: basicInput'
        { validate: note "Name is required." <<< NES.fromString
        , disabled: false
        , ref: RefLabel "name"
        }
    , location: basicInput'
        { validate: note "Location is required." <<< NES.fromString
        , disabled: false
        , ref: RefLabel "location"
        }
    }

  Hooks.useLifecycleEffect do
    form.fields.name.focus
    pure Nothing

  Hooks.pure do
    HH.div_
      [ form.fields.name.input
      , maybe (HH.text "") HH.text form.fields.name.error
      , form.fields.location.input
      , maybe (HH.text "") HH.text form.fields.location.error
      , HH.button
          [ HP.disabled (isNothing form.value || not form.dirty)
          , HE.onClick \e -> Just do
              liftEffect (preventDefault (ME.toEvent e))
              logShow form.value
          ]
          [ HH.text "Submit!" ]
      ]
  where
  basicInput' = basicInput (Proxy2 :: Proxy2 m)
