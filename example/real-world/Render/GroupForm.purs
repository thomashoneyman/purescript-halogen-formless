module Example.RealWorld.Render.GroupForm where

import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (GroupForm)
import Example.RealWorld.Types (GroupCQ, Query, GroupCS)
import Formless as Formless
import Halogen.HTML as HH

render
  :: Formless.State GroupForm Aff
  -> Formless.HTML Query GroupCQ GroupCS GroupForm Aff
render state =
  HH.div_ [ HH.text "I am the group form." ]
