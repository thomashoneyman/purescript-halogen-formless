module Example.RealWorld.Render.OptionsForm where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.RealWorld.Data.Options as O
import Example.RealWorld.Render.Field (text) as Field
import Example.RealWorld.Types (OptionsCQ, Query, OptionsCS)
import Formless as Formless
import Halogen.HTML as HH

render
  :: Formless.State O.OptionsForm Aff
  -> Formless.HTML Query OptionsCQ OptionsCS O.OptionsForm Aff
render state =
  HH.div_
    [ Field.text
      { label: "A text field for costs"
      , placeholder: Just "Fill me in"
      , helpText: "I am actually in the options form."
      , field: O._clickCost
      } state
    ]
