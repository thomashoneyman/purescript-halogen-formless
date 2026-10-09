module Example.LocalStorage where

import Prelude

import Data.Argonaut as Json
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Example.Utils.Field as UI
import Example.Utils.Validation as Validation
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type StringField :: (Type -> Type -> Type -> Type) -> Type
type StringField f = f String String String

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name :: StringField f
  , nickname :: StringField f
  , city :: StringField f
  , state :: StringField f
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | Receive FormContext
  | Eval FormAction

form :: forall query. H.Component query Unit { | Form F.FieldOutput } Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , initialize = Just Initialize
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  key :: String
  key = "local-storage-form"

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Initialize -> do
      storedState <- H.liftEffect $ Storage.getItem key =<< Window.localStorage =<< HTML.window
      case Json.decodeJson =<< Json.parseJson =<< note (Json.TypeMismatch "No data") storedState of
        Left err ->
          Console.log $ Json.printJsonDecodeError err
        Right fields -> do
          setFields <- H.gets _.formActions.setFields
          handleAction $ setFields fields

    Receive context -> do
      let fieldsJson = Json.stringify $ Json.encodeJson context.fields
      H.liftEffect $ Storage.setItem key fieldsJson =<< Window.localStorage =<< HTML.window
      H.put context

    Eval action ->
      F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = F.handleSubmitValidate F.raise F.validate
    { name: Validation.requiredText
    , nickname: Right
    , city: Validation.requiredText
    , state: Validation.requiredText
    }

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ UI.textInput
          { label: "Name"
          , state: fields.name
          , action: actions.name
          }
          [ HP.placeholder "Jack" ]
      , UI.textInput
          { label: "Nickname"
          , state: fields.nickname
          , action: actions.nickname
          }
          []
      , UI.textInput
          { label: "City"
          , state: fields.city
          , action: actions.city
          }
          [ HP.placeholder "Los Angeles" ]
      , UI.textInput
          { label: "State"
          , state: fields.state
          , action: actions.state
          }
          [ HP.placeholder "California" ]
      , HH.br_
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]
