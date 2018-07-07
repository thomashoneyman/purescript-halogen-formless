module Main where

import Prelude

import App.Form as Form
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "external-components" $ proxy Form.component ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook stories body
