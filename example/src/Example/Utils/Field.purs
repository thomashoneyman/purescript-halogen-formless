-- | This module demonstrates how to write custom form fields for your
-- | application on top of the helpers provided by Formless. Most applications
-- | will have a module like this which defines common, reusable form inputs.
module Example.Utils.Field where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Formless (FieldAction, FieldState)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.File.File (File)

type TextInput action output =
  { label :: String
  , state :: FieldState String String output
  , action :: FieldAction action String String output
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { label, state, action } =
  withLabel { label, state } <<< HH.input <<< append
    [ HP.value state.value
    , case state.result of
        Nothing -> HP.attr (HH.AttrName "aria-touched") "false"
        Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
        Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
    , HE.onValueInput action.handleChange
    , HE.onBlur action.handleBlur
    ]

textInput_
  :: forall output action slots m
   . TextInput action output
  -> H.ComponentHTML action slots m
textInput_ = flip textInput []

type Textarea action output =
  { label :: String
  , state :: FieldState String String output
  , action :: FieldAction action String String output
  }

textarea
  :: forall output action slots m
   . Textarea action output
  -> Array (HP.IProp HTMLtextarea action)
  -> H.ComponentHTML action slots m
textarea { label, state, action } =
  withLabel { label, state } <<< HH.textarea <<< append
    [ HP.value state.value
    , HE.onValueInput action.handleChange
    , HE.onBlur action.handleBlur
    ]

textarea_
  :: forall output action slots m
   . Textarea action output
  -> H.ComponentHTML action slots m
textarea_ = flip textarea []

type Checkbox error action =
  { label :: String
  , state :: FieldState Boolean error Boolean
  , action :: FieldAction action Boolean error Boolean
  }

checkbox
  :: forall error action slots m
   . Checkbox error action
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
checkbox { label, state, action } props =
  HH.fieldset_
    [ HH.label_
        [ HH.input $ flip append props
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.value
            , HE.onChecked action.handleChange
            , HE.onBlur action.handleBlur
            ]
        , HH.text label
        ]
    ]

checkbox_
  :: forall error action slots m
   . Checkbox error action
  -> H.ComponentHTML action slots m
checkbox_ = flip checkbox []

type RadioGroup action input output =
  { label :: String
  , state :: FieldState input Void output
  , action :: FieldAction action input Void output
  , options ::
      Array
        { option :: input
        , render :: String
        , props :: Array (HP.IProp HTMLinput action)
        }
  }

radioGroup
  :: forall input output action slots m
   . Eq input
  => RadioGroup action input output
  -> H.ComponentHTML action slots m
radioGroup { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } ->
        HH.label_
          [ HH.input $ flip append props
              [ HP.type_ HP.InputRadio
              , HP.name action.key
              , HP.checked (state.value == option)
              , HE.onChange (\_ -> action.handleChange option)
              , HE.onBlur action.handleBlur
              ]
          , HH.text render
          ]
    ]

type FileUpload action slots m output =
  { label :: String
  , state :: FieldState (Array File) String output
  , action :: FieldAction action (Array File) String output
  , onValid :: output -> H.ComponentHTML action slots m
  }

fileUpload
  :: forall action slots m output
   . FileUpload action slots m output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
fileUpload { label, state, action, onValid } props =
  HH.div_
    [ HH.label
        [ HP.for action.key ]
        [ HH.text label
        , HH.input $ flip append props
            [ HP.name action.key
            , HP.type_ HP.InputFile
            , HE.onFileUpload action.handleChange
            , HE.onBlur action.handleBlur
            ]
        ]
    , case state.result of
        Just (Left error) -> HH.small_ [ HH.text error ]
        Just (Right output) -> HH.small_ [ onValid output ]
        _ -> HH.text ""
    ]

fileUpload_
  :: forall action slots m output
   . FileUpload action slots m output
  -> H.ComponentHTML action slots m
fileUpload_ = flip fileUpload []

type Labelled input output =
  { label :: String
  , state :: FieldState input String output
  }

-- Attach a label and error text to a form input
withLabel
  :: forall input output action slots m
   . Labelled input output
  -> H.ComponentHTML action slots m
  -> H.ComponentHTML action slots m
withLabel { label, state } html =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , html
    , case state.result of
        Just (Left error) -> HH.small_ [ HH.text error ]
        _ -> HH.text ""
    ]
