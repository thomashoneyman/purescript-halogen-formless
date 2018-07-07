module Main where

import Prelude

import Example.Basic.Component as Basic
import Example.ExternalComponents.Component as ExternalComponents
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "basic" $ proxy Basic.component
  , Tuple "external-components" $ proxy ExternalComponents.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook stories body
