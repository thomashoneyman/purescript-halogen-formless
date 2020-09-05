module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (maybe)
import Data.String.NonEmpty as NES
import Data.Variant.Internal (FProxy(..))
import Effect.Class (class MonadEffect)
import Example.UI.TextField (textField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (buildForm, initialFormState, useFormWithState)

basic :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useMyForm

  Hooks.pure do
    HH.div_
      [ form.fields.name.input
      , maybe (HH.text "") HH.text form.fields.name.error
      , form.fields.location.input
      , maybe (HH.text "") HH.text form.fields.location.error
      ]
  where
  textField' = textField (FProxy :: FProxy m)

  useMyForm = useFormWithState (\_ -> initialFormState) $ buildForm
    { name: textField'
        { validate: note "Name is required." <<< NES.fromString
        , disabled: false
        }
    , location: textField'
        { validate: note "Location is required." <<< NES.fromString
        , disabled: false
        }
    }
