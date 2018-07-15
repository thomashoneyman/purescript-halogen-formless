module Example.ExternalComponents.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log) as Console
import Example.ExternalComponents.RenderForm (formless)
import Example.ExternalComponents.Spec (FormRow, _email, _language, _whiskey, formSpec, formValidation)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), Slot(..), State)
import Formless as Formless
import Formless.Spec as FSpec
import Halogen as H
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
    , HH.slot
        unit
        Formless.component
        { formSpec
        , validator: pure <$> formValidation
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
      Formless.Emit q -> eval q *> pure a

      -- When the form is validated, Formless will show the current status of all
      -- fields and will report the total number of errors across fields. Only
      -- validated fields report errors.
      Formless.Validated errors -> do
        H.liftEffect $ Console.log $ "Validated! Errors: " <> show errors
        pure a

      -- We'll just log the result here, but you could send this off to a server for
      -- processing on success.
      Formless.Submitted result -> case result of
        Left f -> do
          H.liftEffect $ Console.log "Failed to validate form."
          pure a
        Right v -> do
          -- Now we have just a simple record of successful output!
          let form :: Record (FormRow FSpec.Output)
              form = FSpec.unwrapOutput v

          H.liftEffect $ do
             Console.log "Successfully validated form."
             Console.log $ "Whiskey: " <> form.whiskey

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
              _ <- H.query unit $ Formless.handleChange _email (Just x)
              _ <- H.query unit $ Formless.handleBlur _email
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query unit $ Formless.handleChange _whiskey (Just x)
              _ <- H.query unit $ Formless.handleBlur _whiskey
              _ <- H.query unit $ Formless.Send EmailTypeahead (TA.ReplaceSelections (TA.One Nothing) unit) unit
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ Formless.handleChange _language (Just x)
              _ <- H.query unit $ Formless.handleBlur _language
              _ <- H.query unit $ Formless.Send EmailTypeahead (TA.ReplaceSelections (TA.One Nothing) unit) unit
              pure a
        _ -> do
          case slot of
            EmailTypeahead -> do
              _ <- H.query unit $ Formless.handleChange _email Nothing
              _ <- H.query unit $ Formless.handleBlur _email
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query unit $ Formless.handleChange _whiskey Nothing
              _ <- H.query unit $ Formless.handleBlur _whiskey
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ Formless.handleChange _language Nothing
              _ <- H.query unit $ Formless.handleBlur _language
              pure a

      -- Unfortunately, single-select typeaheads send blur events before
      -- they send the selected value, which causes validation to run
      -- before the new value is ready to be validated. Item selection
      -- therefore serves as the blur event, too.
      TA.VisibilityChanged _ -> pure a

      -- We care about selections, not searches, so we'll ignore this message.
      TA.Searched _ -> pure a
