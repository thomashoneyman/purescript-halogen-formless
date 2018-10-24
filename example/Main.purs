module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Basic.Component as Basic
import Example.Nested.Component as Nested
import Example.ExternalComponents.Component as ExternalComponents
import Example.App.Home as Home
import Example.RealWorld.Component as RealWorld
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML (text) as HH
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy Home.component
  , Tuple "Basic" $ proxy Basic.component
  , Tuple "External Components" $ proxy ExternalComponents.component
  , Tuple "Nested" $ proxy Nested.component
  , Tuple "Real World" $ proxy RealWorld.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Formless" } body
