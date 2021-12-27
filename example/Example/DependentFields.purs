module Example.DependentFields where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Except as ExceptT
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (codePointFromChar)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Example.Utils.Field as UI
import Example.Utils.Types (Email, Username)
import Example.Utils.Validation as Validation
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Output =
  { email :: Email
  , username :: Username
  , password :: String
  }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( email :: f String String Email
  , username :: f String String Username
  , password1 :: f String String String
  , password2 :: f String String String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

form :: forall query. H.Component query Unit Output Aff
form = F.formless { liftAction: Eval, validateOnModify: true } mempty $ H.mkComponent
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
      validatePasswords
        :: String
        -> { value :: String, result :: Maybe (Either String String), validate :: Action }
        -> H.HalogenM _ _ _ _ _ (Either String String)
      validatePasswords input otherPassword = runExceptT do
        let
          validateEq a b
            | a == b = Right a
            | otherwise = Left "Passwords must match."

        password <- ExceptT.except $ Validation.longerThan 3 =<< Validation.requiredText input
        _ <- ExceptT.except $ validateEq password otherPassword.value
        case otherPassword.result of
          Just (Left _) -> do
            -- If this password is valid and the passwords match, but the other
            -- password field has an error, then it has a stale validation error
            -- for not matching this field. We should therefore trigger
            -- validation on that field.
            ExceptT.lift $ void $ H.fork do
              H.liftAff $ Aff.delay $ Aff.Milliseconds 10.0
              handleAction otherPassword.validate
          _ -> pure unit
        pure password

      -- This validation all runs in `HalogenM`, so you have full access to your
      -- component (including the form itself). Below, we demonstrate some ways
      -- you can use this information.
      validation :: { | Form (F.FieldValidationM (H.HalogenM _ _ _ _ _)) }
      validation =
        { email: \input -> do
            let validated = Validation.email input
            -- If the email address is valid and the username field hasn't been
            -- touched yet, then we'll pre-fill the contents of the username
            -- field on behalf of the user.
            usernameResult <- H.gets _.fields.username.result
            when (isRight validated && isNothing usernameResult) do
              let start = String.takeWhile (_ /= codePointFromChar '@') input
              -- Since we've set `validateOnModify` to be true in our form
              -- config, modifying this field manually will trigger validation
              -- automatically.
              modifyUsername <- H.gets _.actions.username.modify
              handleAction $ modifyUsername _ { value = start }
            pure validated

        , username:
            pure <<< Validation.username

        , password1: \input -> do
            { value, result } <- H.gets _.fields.password2
            { validate } <- H.gets _.actions.password2
            validatePasswords input { value, result, validate }

        , password2: \input -> do
            { value, result } <- H.gets _.fields.password1
            { validate } <- H.gets _.actions.password1
            validatePasswords input { value, result, validate }
        }

      -- We'd like to modify the output of our form to consolidate the duplicate
      -- password fields.
      toOutput :: { | Form F.FieldOutput } -> Output
      toOutput { email, username, password1 } = { email, username, password: password1 }

    F.handleSubmitValidateM (F.raise <<< toOutput) F.validateM validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ UI.textInput
          { label: "Email"
          , state: fields.email
          , action: actions.email
          }
          [ HP.placeholder "jack@kerouac.com" ]
      , UI.textInput
          { label: "Username"
          , state: fields.username
          , action: actions.username
          }
          [ HP.placeholder "jk" ]
      , UI.textInput
          { label: "Password"
          , state: fields.password1
          , action: actions.password1
          }
          [ HP.type_ HP.InputPassword ]
      , UI.textInput
          { label: "Password (Confirm)"
          , state: fields.password2
          , action: actions.password2
          }
          [ HP.type_ HP.InputPassword ]
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]
