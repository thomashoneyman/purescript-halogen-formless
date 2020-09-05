module Example.Basic where

import Prelude

import Data.Either (either, hush, note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormInput(..), buildForm, initialFormState, useFormWithState)

{-
type UseBasicFormInput a =
  UseReactRef (Maybe HTMLElement)
    Hooks.<> Hooks.UseMemo (Either ReactElement a)
    Hooks.<> Hooks.Nil

type BasicFormInputProps a =
  { validate :: String -> Either ReactElement a
  , disabled :: Boolean
  }

type BasicFormInputInterface =
  ( error :: Maybe ReactElement
  , focus :: Effect Unit
  | WithInput ()
  )

basicFormInput
  :: forall a
   . BasicFormInputProps a
  -> FormInput (UseBasicFormInput a) BasicFormInputInterface String a
basicFormInput { validate, disabled } = FormInput \form -> Hooks.do
  let currentValue = fromMaybe "" form.value
  inputRef <- useHTMLElementRef mempty
  isValid <- useValidate currentValue
  Hooks.pure
    { input:
        D.input
          [ P.className "Input"
          , P._type "text"
          , P.value currentValue
          , P.disabled disabled
          , P.onChange (traverse_ form.onChange <=< readTargetInputValue)
          , inputRef.prop
          ]
    , error: preview _Left isValid
    , focus: inputRef.get >>= traverse_ HTMLElement.focus
    , value: foldA isValid
    }
  where
  useValidate value =
    Hooks.captures { value } Hooks.useMemo \_ ->
      validate value
-}

basic :: forall q i o m. H.Component HH.HTML q i o m
basic = Hooks.component \_ _ -> Hooks.do
  form <- useMyForm

  Hooks.pure do
    HH.div_
      [ HH.p_
          [ form.fields.string.input
          , fromMaybe (HH.text "") form.fields.string.error
          ]
      ]
  where
  useMyForm = useFormWithState (\_ -> initialFormState) $ buildForm { string }
    where
    string = FormInput \form -> Hooks.do
      touched /\ modifyTouched <- map (map Hooks.modify_) $ Hooks.useState false

      let
        currentValue =
          fromMaybe "" form.value

        isValid =
          note (HH.text "Required.") (NES.fromString currentValue)

      Hooks.pure
        { input:
            HH.input
              [ HP.value currentValue
              , HE.onValueInput \val -> Just do
                  modifyTouched \_ -> true
                  form.onChange val
              ]
        , error: if touched then either (\e -> Just e) (\_ -> Nothing) isValid else Nothing
        , value: hush isValid
        }

