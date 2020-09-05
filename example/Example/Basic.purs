module Example.Basic where

import Prelude

import Data.Either (note)
import Data.Maybe (maybe)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Effect.Class (class MonadEffect)
import Example.UI.TextField (UseTextField, TextFieldInterface, textField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormInput, buildForm, initialFormState, useFormWithState)

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
  useMyForm = useFormWithState (\_ -> initialFormState) (buildForm myForm)

  myForm =
    { name: do
        let
          field :: FormInput m (UseTextField NonEmptyString) (TextFieldInterface () m) String NonEmptyString
          field = textField
            { validate: note "Name is required." <<< NES.fromString
            , disabled: false
            }
        field
    , location: do
        let
          field :: FormInput m (UseTextField NonEmptyString) (TextFieldInterface () m) String NonEmptyString
          field = textField
            { validate: note "Location is required." <<< NES.fromString
            , disabled: false
            }
        field
    }
