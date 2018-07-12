module Example.RealWorld.Render.OptionsForm where

import Halogen.HTML as HH

render :: forall t1 t2 t3. t1 -> HH.HTML t3 t2
render state =
  HH.div_ []
