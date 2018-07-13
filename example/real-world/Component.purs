module Example.RealWorld.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.RealWorld.Data.Group (GroupFormRow)
import Example.RealWorld.Data.Options (OptionsRow)
import Example.RealWorld.Render.GroupForm (render) as GroupForm
import Example.RealWorld.Render.Nav (tabs) as Nav
import Example.RealWorld.Render.OptionsForm (render) as OptionsForm
import Example.RealWorld.Spec.GroupForm (groupFormSpec, groupFormValidation)
import Example.RealWorld.Spec.OptionsForm (optionsFormSpec, optionsFormValidation)
import Example.RealWorld.Types (ChildQuery, ChildSlot, Query(..), State, Tab(..))
import Formless as Formless
import Formless.Spec as FSpec
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA
import Ocelot.HTML.Properties (css)

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.parentComponent
  { initialState: const { focus: GroupFormTab }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.div
    [ css "flex-1 container p-12" ]
    [ Format.heading_
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A complex form inspired by real-world use cases." ]
    , Format.p_
      [ HH.text $
        "This component demonstrates building a large form with complex rendering and validation "
        <> "requirements. Notice how both tabs end up unifying to a single output type after the "
        <> "two forms are combined, how various dropdowns determine the contents (and visibility) "
        <> "of other form elements, the assorted external components, and how validation for many "
        <> "fields depends on the values of other fields in the form."
      ]
    , Format.p_
      [ HH.text $
        "Next, review the source code. You'll notice that all of the complex types and state necessary "
        <> "to run this form can be generated from a pair of row types. All that's left for you to handle "
        <> "is to write the validation (with helper functions) and the render function."
      ]
    , Nav.tabs st
    , case st.focus of
        GroupFormTab ->
          HH.slot'
            CP.cp1
            unit
            Formless.component
            { formSpec: groupFormSpec
            , validator: pure <$> groupFormValidation
            , render: GroupForm.render
            }
            (HE.input HandleGroupForm)
        OptionsFormTab ->
          HH.slot'
            CP.cp2
            unit
            Formless.component
            { formSpec: optionsFormSpec
            , validator: pure <$> optionsFormValidation
            , render: OptionsForm.render
            }
            (HE.input HandleOptionsForm)
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    Select tab a -> do
      H.modify_ _ { focus = tab }
      pure a

    HandleGroupForm m a -> case m of
      Formless.Emit q -> eval q *> pure a

      Formless.Submitted result -> case result of
        Left f -> do
          H.liftEffect $ Console.log "Failed to validate form."
          pure a
        Right v -> do
          -- Now we have just a simple record of successful output!
          let form :: Record (GroupFormRow FSpec.Output)
              form = FSpec.unwrapOutput v

          H.liftEffect $ do
             Console.log "Successfully validated form."
             Console.log $ "Whiskey: " <> form.whiskey
          pure a

    HandleOptionsForm m a -> case m of
      Formless.Emit q -> eval q *> pure a

      Formless.Submitted result -> case result of
        Left f -> do
          H.liftEffect $ Console.log "Failed to validate form."
          pure a
        Right v -> do
          -- Now we have just a simple record of successful output!
          let form :: Record (OptionsRow FSpec.Output)
              form = FSpec.unwrapOutput v

          H.liftEffect $ do
             Console.log "Successfully validated form."
             Console.log $ "Enabled: " <> show form.enable
          pure a

    HandleTypeahead slot m a -> case m of
      TA.Emit q -> eval q *> pure a
      TA.SelectionsChanged s _ -> pure a
      TA.VisibilityChanged _ -> pure a
      TA.Searched _ -> pure a
