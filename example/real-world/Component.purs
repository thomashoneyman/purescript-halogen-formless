module Example.RealWorld.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.RealWorld.Data.Group (GroupFormRow)
import Example.RealWorld.Data.Options (OptionsRow)
import Example.RealWorld.Render.GroupForm (render) as GroupForm
import Example.RealWorld.Render.OptionsForm (render) as OptionsForm
import Example.RealWorld.Spec.GroupForm (groupFormSpec, groupFormValidation)
import Example.RealWorld.Spec.OptionsForm (optionsFormSpec, optionsFormValidation)
import Example.RealWorld.Types (ChildQuery, ChildSlot, Query(..), State)
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
    { initialState: const unit
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
      [ HH.text "A form leveraging external components and custom form actions." ]
    , Format.p_
      [ HH.text $
        "In Formless, you can freely leverage external components and embed them in the form. "
        <> "This form shows how to use external typeaheads from the Ocelot design system from "
        <> "CitizenNet. This form also demonstrates how you can manipulate forms in Formless. "
        <> "Try selecting an email address, then a whiskey. You'll notice that changing your "
        <> "whiskey selection also clears the selected email."
      ]
    , Format.p_
      [ HH.text $
        "Next, try opening the console. If you submit the form with invalid values, Formless will "
        <> "show you your errors. If you submit a valid form, you'll see Formless just returns the "
        <> "valid outputs for you to work with."
      ]
    , HH.slot'
        CP.cp1
        unit
        Formless.component
        { formSpec: groupFormSpec
        , validator: pure <$> groupFormValidation
        , render: GroupForm.render
        }
        (HE.input HandleGroupForm)
    , HH.slot'
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
    -- Always have to handle the `Emit` case
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
