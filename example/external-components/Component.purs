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
    HandleFormless m a -> a <$ case m of
      F.Emit q -> eval q
      F.Submitted user -> do
        H.liftEffect $ Console.log $ show (user :: User)
      F.Changed fstate -> do
        H.liftEffect $ Console.log $ show $ delete (SProxy :: SProxy "form") fstate

    Reset a -> do
      _ <- H.query unit $ H.action $ F.Send EmailTypeahead clear
      _ <- H.query unit $ H.action $ F.Send WhiskeyTypeahead clear
      _ <- H.query unit $ H.action $ F.Send LanguageTypeahead clear
      _ <- H.query unit $ H.action F.ResetAll
      pure a

    HandleTypeahead slot m a -> case m of
      TA.Emit q -> eval q $> a
      TA.SelectionsChanged s _ -> case s of
        TA.ItemSelected x -> do
          case slot of
            EmailTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _email (Just x))
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _whiskey (Just x))
              _ <- H.query unit $ H.action $ F.Send EmailTypeahead clear
              _ <- H.query unit $ H.action $ F.Reset (F.resetField _email)
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _language (Just x))
              _ <- H.query unit $ H.action $ F.Send EmailTypeahead clear
              _ <- H.query unit $ H.action $ F.Reset (F.resetField _email)
              pure a
        _ -> do
          case slot of
            EmailTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _email Nothing)
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _whiskey Nothing)
              pure a
            LanguageTypeahead -> do
              _ <- H.query unit $ H.action $ F.ModifyValidate (F.setInput _language Nothing)
              pure a
      TA.VisibilityChanged _ -> pure a
      TA.Searched _ -> pure a

  clear = H.action $ TA.ReplaceSelections (TA.One Nothing)
