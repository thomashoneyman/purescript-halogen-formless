module Example.RealWorld.Render.Nav where

import Prelude

import Effect.Aff (Aff)
import Example.RealWorld.Types (ChildQuery, ChildSlot, Query(..), State, Tab(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.HTML.Properties (css)

tabs :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
tabs state =
  HH.ul
  [ css "list-reset flex" ]
  [ HH.li
    [ css "mr-3" ]
    [ HH.p
      [ if state.focus == GroupFormTab then css selected else css notSelected
      , HE.onClick $ HE.input_ $ Select GroupFormTab ]
      [ HH.text "Group Form" ]
    ]
  , HH.li
    [ css "mr-3" ]
    [ HH.p
      [ if state.focus == OptionsFormTab then css selected else css notSelected
      , HE.onClick $ HE.input_ $ Select OptionsFormTab ]
      [ HH.text "Options Form" ]
    ]
  ]
  where
    selected = "inline-block border rounded py-1 px-3 bg-black text-white"
    notSelected = "inline-block border rounded text-black py-1 px-3"

