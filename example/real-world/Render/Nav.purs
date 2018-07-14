module Example.RealWorld.Render.Nav where

import Prelude

import Effect.Aff (Aff)
import Example.RealWorld.Types (ChildQuery, ChildSlot, Query(..), State, Tab(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Button (button, buttonDark, buttonPrimary) as Button
import Ocelot.HTML.Properties (css)

tabs :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
tabs state =
  HH.ul
  [ css "list-reset flex" ]
  [ HH.li
    [ css "mr-3" ]
    [ if state.focus == GroupFormTab
        then group Button.button
        else group Button.buttonDark
    ]
  , HH.li
    [ css "mr-3" ]
    [ if state.focus == OptionsFormTab
        then options Button.button
        else options Button.buttonDark
    ]
  , HH.li
    [ css "mr-3" ]
    [ Button.buttonPrimary
      [ HE.onClick $ HE.input_ Submit ]
      [ HH.text "Submit Form" ]
    ]
  ]
  where
    group f = f
      [ HE.onClick $ HE.input_ $ Select GroupFormTab ]
      [ HH.text "Group Form" ]

    options f = f
      [ HE.onClick $ HE.input_ $ Select OptionsFormTab ]
      [ HH.text "Options Form" ]
