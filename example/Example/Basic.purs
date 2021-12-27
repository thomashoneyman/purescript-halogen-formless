-- | A basic example that doesn't use app-specific fields or validators, showing
-- | how you would manually wire up a form without any helpers on top of
-- | Formless itself.
module Example.Basic where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name :: f String String String
  , message :: f String Void String
  )

type FormlessAction = F.FormlessAction (Form F.FieldState)
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action

data Action
  = Receive FormContext
  | Eval FormlessAction

form :: forall query. H.Component query Unit { | Form F.FieldOutput } Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        { name: case _ of
            "" -> Left "Required"
            val -> Right val
        , message: Right
        }

    F.handleSubmitValidate F.raise F.validate validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ HH.label_ [ HH.text "Name" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.name.handleChange
              , HE.onBlur actions.name.handleBlur
              , case fields.name.result of
                  Nothing -> HP.placeholder "Jack"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.name.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      , HH.div_
          [ HH.label_ [ HH.text "Message" ]
          , HH.textarea
              [ HE.onValueInput actions.message.handleChange
              , HE.onBlur actions.message.handleBlur
              ]
          ]
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]
