module Example.ExternalComponents.Form where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as TA
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (delete)
import Select as Select

-- Form spec

-- equivalent to { name :: String, email :: V.Email, ... }
type User = { | UserFormRow F.OutputType }

newtype UserForm r f = UserForm (r (UserFormRow f))
derive instance newtypeUserForm' :: Newtype (UserForm r f) _

type UserFormRow f =
  ( name     :: f V.FieldError String         String
  , email    :: f V.FieldError (Maybe String) V.Email
  , whiskey  :: f V.FieldError (Maybe String) String
  , language :: f V.FieldError (Maybe String) String
  )


-- Form component types

data Action
  = HandleTypeahead Typeahead (TA.Message Maybe String)
  | Reset


-- Form child component types

type ChildSlots =
  ( typeahead :: TA.Slot Maybe String Typeahead )

data Typeahead
  = Email
  | Whiskey
  | Language

derive instance eqTypeahead :: Eq Typeahead
derive instance ordTypeahead :: Ord Typeahead

-- Component spec

component :: F.Component UserForm (Const Void) ChildSlots Unit User Aff
component = F.component (const defaultInput) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  defaultInput :: F.Input' UserForm Aff
  defaultInput =
    { validators: UserForm
        { name: V.minLength 7
        , email: V.exists >>> V.emailFormat
        , whiskey: V.exists
        , language: V.exists
        }
    , initialInputs: Nothing
    }

  handleEvent = case _ of
    F.Submitted outputs -> H.raise (F.unwrapOutputFields outputs)
    F.Changed formState -> logShow $ delete (SProxy :: _ "form") formState

  prx = F.mkSProxies (F.FormProxy :: _ UserForm)

  handleAction = case _ of
    HandleTypeahead slot (TA.SelectionsChanged new) -> case slot of
      Email ->
        eval $ F.setValidate prx.email new

      Whiskey -> do
        eval $ F.setValidate prx.whiskey new
        eval $ F.reset prx.email
        void $ H.query TA._typeahead Email TA.clear

      Language -> do
        eval $ F.setValidate prx.language new

    Reset -> do
      items <- H.query TA._typeahead Email $ H.request TA.getAvailableItems
      logShow $ fromMaybe [] items
      _ <- H.queryAll TA._typeahead TA.clear
      eval F.resetAll

    where
    -- you will usually want to define this pre-applied function if you
    -- are recursively evaluating Formless actions.
    eval act = F.handleAction handleAction handleEvent act

  render :: F.PublicState UserForm () -> F.ComponentHTML UserForm Action ChildSlots Aff
  render st =
    UI.formContent_
      [ name
      , email
      , whiskey
      , language
      , UI.p_
          """
          You can only attempt to submit this form if it is valid and not already being submitted. You can only attempt to reset the form if it has changed from its initial state.
          """
      , HH.br_
      , UI.grouped_
          [ UI.buttonPrimary
              [ if st.submitting || st.validity /= F.Valid
                  then HP.disabled true
                  else HE.onClick \_ -> Just F.submit
              ]
              [ HH.text "Submit" ]
          , UI.button
              [ if not st.dirty
                  then HP.disabled true
                  else HE.onClick \_ -> Just $ F.injAction Reset
              ]
              [ HH.text "Reset" ]
          ]
      ]
    where
    name = st # UI.formlessField UI.input
      { label: "Name"
      , help: "Write your name"
      , placeholder: "Dale"
      , sym: prx.name
      }

    email = UI.field
      { label: "Email"
      , help: F.getResult prx.email st.form # UI.resultToHelp "Choose an email"
      }
      [ singleTypeahead Email
          { placeholder: "me@you.com"
          , items:
              [ "not@anemail.org"
              , "snail@utopia.snailutopia"
              , "blue@jordans@blordens.pordens"
              , "yea_that_won't_work@email.com"
              , "standard@email.com"
              ]
          }
      ]

    whiskey = UI.field
      { label: "Whiskey"
      , help: F.getResult prx.whiskey st.form # UI.resultToHelp
          "Select a favorite whiskey"
      }
      [ singleTypeahead Whiskey
          { placeholder: "Lagavulin 12"
          , items:
              [ "Lagavulin 16"
              , "Kilchoman Blue Label"
              , "Laphroaig"
              , "Ardbeg"
              ]
          }
      ]

    language = UI.field
      { label: "Language"
      , help: F.getResult prx.language st.form # UI.resultToHelp
          "Choose your favorite programming language"
      }
      [ singleTypeahead Language
          { placeholder: "Haskell"
          , items:
              [ "Rust"
              , "Python"
              , "Blodwen"
              , "Hackett"
              , "PHP"
              , "PureScript"
              , "JavaScript"
              , "C"
              , "C++"
              , "TLA+"
              , "F#"
              , "F*"
              , "Agda"
              , "Ruby"
              , "APL"
              ]
          }
      ]

    singleTypeahead slot input =
      HH.slot TA._typeahead slot (Select.component TA.single) (TA.input input) handler
      where
      handler = Just <<< F.injAction <<< HandleTypeahead slot
