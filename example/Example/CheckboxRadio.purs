module Example.CheckboxRadio where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.Utils.Field as UI
import Example.Utils.Types (Picked(..))
import Example.Utils.Validation as Validation
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name :: f String String String
  , subscribe :: f Boolean Void Boolean
  , picked :: f Picked Void Picked
  )

type FormContext =
  F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action

data Action
  = Receive FormContext
  | Eval (F.FormlessAction (Form F.FieldState))

form :: forall query. H.Component query Unit { | Form F.FieldOutput } Aff
form = F.formless { liftAction: Eval } initialForm $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  initialForm :: { | Form F.FieldInput }
  initialForm = { name: "", subscribe: false, picked: One }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = F.handleSubmitValidate F.raise F.validate
    { name: Validation.requiredText
    , subscribe: Right
    , picked: Right
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
      , UI.checkbox_
          { label: "Subscribe"
          , state: fields.subscribe
          , action: actions.subscribe
          }
      , UI.radioGroup
          { label: "Pick One"
          , options:
              [ { option: One, render: "One", props: [] }
              , { option: Two, render: "Two", props: [] }
              , { option: Three, render: "Three", props: [] }
              ]
          , state: fields.picked
          , action: actions.picked
          }
      , HH.br_
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]
