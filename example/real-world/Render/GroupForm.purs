module Example.RealWorld.Render.GroupForm where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group as G
import Example.RealWorld.Render.Field (text) as Field
import Example.RealWorld.Types (GroupCQ, Query, GroupCS)
import Formless as Formless
import Halogen.HTML as HH

render
  :: Formless.State G.GroupForm Aff
  -> Formless.HTML Query GroupCQ GroupCS G.GroupForm Aff
render state =
  HH.div_
    [ Field.text
      { label: "A text field"
      , placeholder: Just "Fill me in"
      , helpText: "I exist on the group form."
      , field: G._name
      } state
    ]
