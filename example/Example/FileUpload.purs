module Example.FileUpload where

import Prelude

import Data.Array (elem)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common as MediaType
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Example.Utils.Field as UI
import Example.Utils.Validation as Validation
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.File.File (File)
import Web.File.File as File

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name :: f String String String
  , photo :: f (Array File) String { name :: String, size :: Number }
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
  handleQuery = F.handleSubmitValidate F.raise F.validate
    { name: Validation.requiredText
    , photo: validateFileSize <=< validateFileType <=< validateFileCount
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
      , UI.fileUpload_
          { label: "Upload Photo"
          , state: fields.photo
          , action: actions.photo
          , onValid: \{ size } -> HH.small_ [ HH.text $ "Your photo size: " <> show size ]
          }
      , HH.br_
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]

validateFileCount :: Array File -> Either String File
validateFileCount files = case Array.uncons files of
  Just { head, tail: [] } -> Right head
  Just _ -> Left "Only one photo can be uploaded."
  Nothing -> Left "Required."

validateFileType :: File -> Either String File
validateFileType file = case File.type_ file of
  Nothing -> Left "Unrecognized file type. Accepted types: png, jpeg, gif."
  Just ty | ty `elem` [ MediaType.imagePNG, MediaType.imageJPEG, MediaType.imageGIF ] -> Right file
  Just ty -> Left ("Unsupported file type: " <> unwrap ty <> ". Accepted types: png, jpeg, gif.")

validateFileSize :: File -> Either String { name :: String, size :: Number }
validateFileSize file = case File.size file of
  size
    | size > 99_999.0 -> Left "Photos must be smaller than 100kb."
    | size < 1000.0 -> Left "Photos cannot be smaller than 1kb."
    | otherwise -> Right { name: File.name file, size }
