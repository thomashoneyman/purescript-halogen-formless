module Example.RealWorld.Render.OptionsForm where

import Effect.Aff (Aff)
import Example.RealWorld.Data.Options (OptionsForm)
import Example.RealWorld.Types (OptionsCQ, Query, OptionsCS)
import Formless as Formless
import Halogen.HTML as HH

render
  :: Formless.State OptionsForm Aff
  -> Formless.HTML Query OptionsCQ OptionsCS OptionsForm Aff
render state =
  HH.div_ [ HH.text "I am the options form." ]
