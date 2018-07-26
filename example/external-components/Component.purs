module Example.ExternalComponents.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.ExternalComponents.RenderForm (formless)
import Example.ExternalComponents.Spec (User, _email, _language, _whiskey, formSpec, submitter, validator)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), Slot(..), State)
import Formless as F
import Formless.Events as FE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA
import Ocelot.HTML.Properties (css)
import Record (delete)

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
    , HH.slot
        unit
        F.component
        { formSpec
        , validator: pure <$> validator
        , submitter
        , render: formless
        }
        (HE.input HandleFormless)
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    -- Always have to handle the `Emit` case
    HandleFormless m a -> case m of
      -- This is a renderless component, so we must handle the `Emit` case by recursively
      -- calling `eval`
      F.Emit q -> eval q *> pure a

      -- Formless will provide your result type on successful submission.
      F.Submitted user -> do
        H.liftEffect $ Console.log $ show (user :: User)
        pure a

      -- Formless will alert you with the new summary state if it is changed.
      F.Changed fstate -> do
        H.liftEffect $ Console.log $ show $ delete (SProxy :: SProxy "form") fstate
        pure a

    -- While Formless is perfectly capable of resetting all fields to their initial
    -- values, it isn't aware of your external components, so you'll need to reset
    -- those yourself.
    Reset a -> do
      _ <- H.query unit $ H.action $ F.Send EmailTypeahead (TA.ReplaceSelections (TA.One Nothing) unit)
      _ <- H.query unit $ H.action $ F.Send WhiskeyTypeahead (TA.ReplaceSelections (TA.One Nothing) unit)
      _ <- H.query unit $ H.action $ F.Send LanguageTypeahead (TA.ReplaceSelections (TA.One Nothing) unit)
      _ <- H.query unit $ H.action F.Reset
      pure a

    HandleTypeahead slot m a -> case m of
      -- This is a renderless component, so we must handle the `Emit` case by recursively
      -- calling `eval`
      TA.Emit q -> eval q *> pure a

      -- We'll use the component output to handle validation and change events. This
      -- is also a nice way to demonstrate how to support custom behavior: you can
      -- send queries through the Formless component to a child, and have the results
      -- raised back up to you.
      TA.SelectionsChanged s _ -> case s of
        TA.ItemSelected x -> do
          case slot of
            EmailTypeahead -> do
              _ <- H.query unit $ FE.handleBlurAndChange _email (Just x)
              pure a
            WhiskeyTypeahead -> do
              -- We can use handleBlurAndChange to manage our component updates
              _ <- H.query unit $ FE.handleBlurAndChange _whiskey (Just x)
              -- This is how you can clear a typeahead via Formless using your queries
              _ <- H.query unit $ F.Send EmailTypeahead (TA.ReplaceSelections (TA.One Nothing) unit) unit
              -- To reset a field, including 'touched' state, errors, and inputs, use handleReset
              _ <- H.query unit $ FE.handleReset _email
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ FE.handleBlurAndChange _language (Just x)
              _ <- H.query unit $ F.Send EmailTypeahead (TA.ReplaceSelections (TA.One Nothing) unit) unit
              _ <- H.query unit $ FE.handleReset _email
              pure a
        _ -> do
          case slot of
            EmailTypeahead -> do
              _ <- H.query unit $ FE.handleBlurAndChange _email Nothing
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query unit $ FE.handleBlurAndChange _whiskey Nothing
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ FE.handleBlurAndChange _language Nothing
              pure a

      -- Unfortunately, single-select typeaheads send blur events before
      -- they send the selected value, which causes validation to run
      -- before the new value is ready to be validated. Item selection
      -- therefore serves as the blur event, too.
      TA.VisibilityChanged _ -> pure a

      -- We care about selections, not searches, so we'll ignore this message.
      TA.Searched _ -> pure a
