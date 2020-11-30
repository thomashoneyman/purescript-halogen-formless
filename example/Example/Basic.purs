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
import Halogen.Hooks.Formless (buildForm, initialFormState, useForm)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  -- We are using the compiler to infer our form type. When doing this, we need
  -- to provide a proxy for our monad type, `m`, to each of our form inputs. This
  -- aids the compiler in type inference.
  let proxy = Proxy2 :: Proxy2 m

  form <- useForm (\_ -> initialFormState) $ buildForm
    { name: basicField proxy
        { validate: note "Name is required." <<< NES.fromString
        , initialValue: Just "Tom"
        }
    , message: basicField proxy
        { validate: pure
        , initialValue: Nothing
        }
    }

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
