module Example.ExternalComponents.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Example.ExternalComponents.RenderFormless (formless)
import Example.ExternalComponents.Spec (_email, formSpec)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), State)
import Formless as Formless
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
      [ HH.text "A form leveraging external components." ]
    , HH.slot
        unit
        Formless.component
        { formSpec
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

      -- We'll just log the result here, but you could send this off to a server for
      -- processing on success.
      Formless.Submitted result -> case result of
        Left f -> do
          let _ = spy "Failed to validate form." f
          pure a
        Right v -> do
          let _ = spy "Form is valid!" v
          pure a

    HandleTypeahead m a -> case m of
      -- This is a renderless component, so we must handle the `Emit` case by recursively
      -- calling `eval`
      TA.Emit q -> eval q *> pure a

      -- We'll use the component output to handle validation and change events.
      TA.SelectionsChanged s _ -> case s of
        TA.ItemSelected x -> do
          _ <- H.query unit $ Formless.handleChange _email (Just x)
          _ <- H.query unit $ Formless.handleBlur _email
          pure a
        _ -> do
          _ <- H.query unit $ Formless.handleChange _email Nothing
          _ <- H.query unit $ Formless.handleBlur _email
          pure a

      -- Unfortunately, single-select typeaheads send blur events before
      -- they send the selected value, which causes validation to run
      -- before the new value is ready to be validated. Item selection
      -- therefore serves as the blur event, too.
      TA.VisibilityChanged _ -> pure a

      -- We care about selections, not searches, so we'll ignore this message.
      TA.Searched _ -> pure a

