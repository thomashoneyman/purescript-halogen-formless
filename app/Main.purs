module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH

import Formless (component)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component { render: const $ HH.div_ [ HH.text "Renderless" ] } body
