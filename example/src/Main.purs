module Example.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Example.Basic as Basic
import Example.CheckboxRadio as CheckboxRadio
import Example.DependentFields as DependentFields
import Example.FileUpload as FileUpload
import Example.LocalStorage as LocalStorage
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories)
import Halogen.Storybook as Storybook
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = launchAff_ do
  _ <- HA.awaitLoad
  mbApp <- HA.selectElement (QuerySelector ".app")
  for_ mbApp \app ->
    Storybook.runStorybook { stories, logo: Just $ HH.text "Formless" } app

stories :: Stories Aff
stories = Object.fromFoldable
  [ home
  , basic
  , checkboxRadio
  , fileUpload
  , dependentFields
  , localStorage
  ]
  where
  home = do
    let
      title = HH.h1_ [ HH.text "Formless" ]
      description = HH.p_ [ HH.text "A simple library for writing forms in Halogen" ]
      render = HH.article_ [ title, description ]
      component = H.mkComponent { initialState: identity, render: \_ -> render, eval: H.mkEval H.defaultEval }

    Tuple "" $ Storybook.proxy component

  basic = do
    let
      title = "Basic"
      description = "A simple form that implements all fields manually, without app-specific helpers. Useful to see exactly how Formless should be used when implementing your own helper functions for your application."
      component = mkExample title description Basic.form

    Tuple ("1. " <> title) $ Storybook.proxy component

  checkboxRadio = do
    let
      title = "Checkbox & Radio"
      description = "A form demonstrating how to use common form controls like checkboxes and radio groups. Useful to see how to implement your own form field helpers for your application."
      component = mkExample title description CheckboxRadio.form

    Tuple ("2. " <> title) $ Storybook.proxy component

  fileUpload = do
    let
      title = "File Upload"
      description = "A form with a file upload button and several validation functions. Useful to see how to implement more complex form fields and validation."
      component = mkExample title description FileUpload.form

    Tuple ("3. " <> title) $ Storybook.proxy component

  dependentFields = do
    let
      title = "Dependent Fields"
      description = "A form with fields that can set other field values and fields that mutually depend on each other for validation. Useful to see how to implement validation that accesses the form state and performs effects, as well as how to imperatively modify field states."
      component = mkExample title description DependentFields.form

    Tuple ("4. " <> title) $ Storybook.proxy component

  localStorage = do
    let
      title = "Local Storage"
      description = "A form that persists its state to local storage. Click the submit button and then refresh the page! Useful to see how to imperatively set the form state."
      component = mkExample title description LocalStorage.form

    Tuple ("5. " <> title) $ Storybook.proxy component

mkExample
  :: forall q i o result
   . Show result
  => String
  -> String
  -> H.Component q Unit result Aff
  -> H.Component q i o Aff
mkExample title description formComponent = H.mkComponent
  { initialState: \_ -> { result: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction result =
    H.modify_ _ { result = Just result }

  render state =
    HH.article_
      [ HH.h1_ [ HH.text title ]
      , HH.p_ [ HH.text description ]
      , HH.slot (Proxy :: Proxy "inner") unit formComponent unit identity
      , case state.result of
          Nothing -> HH.text ""
          Just result -> HH.code_ [ HH.text $ show result ]
      ]
