module Example.RealWorld.Render.Nav where

import Prelude

import Effect.Aff (Aff)
import Example.RealWorld.Types (ChildQuery, ChildSlot, Query(..), State, Tab(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
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
  , HH.li
    [ css "mr-3" ]
    [ reset Button.buttonPrimary ]
  ]
  where
    group f = f
      [ HE.onClick $ HE.input_ $ Select GroupFormTab ]
      [ HH.text $ "Group Form" <>
          if state.groupFormErrors > 0
            then " (" <> show state.groupFormErrors  <> ")"
            else ""
      ]

    options f = f
      [ HE.onClick $ HE.input_ $ Select OptionsFormTab ]
      [ HH.text $ "Options Form" <>
          if state.optionsFormErrors > 0
            then " (" <> show state.optionsFormErrors  <> ")"
            else ""
      ]

    -- You can only reset the form if it's been changed in the first
    -- place.
    reset f = f
      [ if state.groupFormDirty || state.optionsFormDirty
          then HE.onClick $ HE.input_ Reset
          else HP.disabled true
      ]
      [ HH.text "Reset All" ]
