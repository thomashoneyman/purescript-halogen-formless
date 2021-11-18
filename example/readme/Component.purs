-- | This example component shows how submitPreventDefault works in Halogen Formless
module Example.Readme.Component (component) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.Validation (class ToText)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Dog = { name :: String, age :: Age }

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _

instance showAge :: Show Age where
  show = show <<< unwrap

data AgeError = TooLow | TooHigh | InvalidInt

newtype DogForm (r :: Row Type -> Type) f = DogForm (r
  --          error    input  output
  ( name :: f Void     String String
  , age  :: f AgeError String Age
  ))

derive instance newtypeDogForm :: Newtype (DogForm r f) _

instance ToText AgeError where
  toText = case _ of
    InvalidInt -> "Age must be an integer"
    TooLow -> "Age cannot be negative"
    TooHigh -> "No dog has lived past 30 before"

input :: forall m. Monad m => F.Input' DogForm m
input =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators: DogForm
      { name: F.noValidation
      , age: F.hoistFnE_ \str -> case Int.fromString str of
          Nothing -> Left InvalidInt
          Just n
            | n < 0 -> Left TooLow
            | n > 30 -> Left TooHigh
            | otherwise -> Right (Age n)
      }
  }

spec :: forall input m. Monad m => F.Spec' DogForm Dog input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    UI.formContent_
      [ HH.form
        [ -- Using a form forces us to deal with an event. Using '\_ -> F.submit' here
          -- would fire the event and cause the page to reload. Instead, we use
          -- 'F.submitPreventDefault' to avoid firing the event unnecessarily
          HE.onSubmit F.submitPreventDefault
        ]
        [ UI.input
              { label: "Name"
              , help: Right "Write your dog's name"
              , placeholder: "Mila"
              }
              [ HP.value $ F.getInput _name st.form
              , HE.onValueInput (F.setValidate _name)
              ]
        , UI.input
              { label: "Age"
              , help: UI.resultToHelp "Write your dog's age" $ F.getResult _age st.form
              , placeholder: "3"
              }
              [ HP.value $ F.getInput _age form
              , HE.onValueInput $ F.setValidate _age
              ]
        , UI.buttonPrimary
            []
            [ HH.text "Submit" ]
        ]
      ]
    where
    _name = Proxy :: Proxy "name"
    _age = Proxy :: Proxy "age"

data Action = HandleDogForm Dog

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction (HandleDogForm dog) = logShow (dog :: Dog)

  render = 
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "The form from the readme" ]
      , HH.slot F._formless unit (F.component (const input) spec) unit HandleDogForm
      ]
