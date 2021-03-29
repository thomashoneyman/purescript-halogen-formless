-- | This example demonstrates a form in which fields depend on other fields to
-- | produce their outputs.
module Example.Dependency where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (fromMaybe)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Field.Basic.Text (textField)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (buildForm, initialFormState, useFormFields, useFormState)
import Type.Proxy (Proxy2(..))
import Web.Event.Event (preventDefault)

mutualDependency :: forall q i o m. MonadEffect m => H.Component q i o m
mutualDependency = Hooks.component \_ _ -> Hooks.do
  formState /\ modifyFormState <- useFormState (\_ -> initialFormState)

  form <- useFormFields (formState /\ modifyFormState) $ buildForm
    { name: textField proxy
        { validate: note "Required" <<< NES.fromString }
    , password: textField proxy
        { validate: note "Required" <<< NES.fromString }
    , confirm: textField proxy
        { validate: \str -> do
          nes <- note "Required" $ NES.fromString str
          if fromMaybe "" formState.form.password == str then
            pure nes
          else
            Left "Passwords don't match."
        }
    }

  Hooks.pure do
    HH.form
      [ HE.onSubmit \ev -> (liftEffect $ preventDefault ev) ]
      [ form.fields.name.input
      , form.fields.password.input
      , form.fields.confirm.input
      ]
  where
  proxy = Proxy2 :: Proxy2 m
