module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Home as Home
import Example.Basic.Component as Basic
import Example.ExternalComponents.Component as ExternalComponents
import Example.CustomBehavior.Component as CustomBehavior
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML (text) as HH
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy Home.component
  , Tuple "basic" $ proxy Basic.component
  , Tuple "external-components" $ proxy ExternalComponents.component
  , Tuple "custom-behavior" $ proxy CustomBehavior.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Formless" } body
