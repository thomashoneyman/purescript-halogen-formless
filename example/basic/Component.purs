module Example.Basic.Component where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Contact = { name :: String, text :: String }

newtype ContactForm (r :: Row Type -> Type) f = ContactForm (r
  ( name :: f V.FieldError String String
  , text :: f Void String String
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

data Action = HandleContact Contact

component :: H.Component (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleContact contact -> H.liftEffect $ logShow (contact :: Contact)

  render =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A basic contact form." ]
      , UI.p_
          """
          You can create a full Halogen contact form like this in less than 20 lines of Formless, excluding the render function.  It's type-safe, supports complex types, has validation, and parses to the output type of your choice."
          """
      , HH.br_
      , HH.slot F._formless unit formComponent unit HandleContact
      ]

  formComponent :: F.Component ContactForm (Const Void) () Unit Contact Aff
  formComponent = F.component (const formInput) $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput =
      { validators: ContactForm { name: V.minLength 5, text: F.noValidation }
      , initialInputs: Nothing
      }

    renderFormless st =
     UI.formContent_
       [ UI.input
           { label: "Name"
           , help: UI.resultToHelp "Write your name" $ F.getResult _name st.form
           , placeholder: "Dale"
           }
           [ HP.value $ F.getInput _name st.form
           , HE.onValueInput (F.setValidate _name)
           ]
       , UI.textarea
           { label: "Message"
           , help: Right "Write us a message"
           , placeholder: "We prefer nice messages, but have at it."
           }
           [ HP.value $ F.getInput _text st.form
           , HE.onValueInput (F.set _text)
           ]
       , UI.buttonPrimary
           [ HE.onClick \_ -> F.submit ]
           [ HH.text "Submit" ]
       ]
     where
     _name = Proxy :: Proxy "name"
     _text = Proxy :: Proxy "text"
