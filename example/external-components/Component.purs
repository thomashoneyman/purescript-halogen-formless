module Example.ExternalComponents.Component where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as TA
import Example.ExternalComponents.RenderForm (formless)
import Example.ExternalComponents.Spec (User, prx, initialInputs, validators)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), Slot(..), State)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
    UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A form leveraging external components and custom form actions." ]
    , UI.p_ $
        "In Formless, you can freely leverage external components and embed them in the form. "
        <> "This form shows how to use custom typeahead components built with Select from "
        <> "CitizenNet. This form also demonstrates how you can manipulate forms in Formless. "
        <> "Try selecting an email address, then a whiskey. You'll notice that changing your "
        <> "whiskey selection also clears the selected email."
    , HH.br_
    , UI.p_ $
        "Next, try opening the console. If you submit the form with invalid values, Formless will "
        <> "show you your errors. If you submit a valid form, you'll see Formless just returns the "
        <> "valid outputs for you to work with."
    , HH.br_
    , HH.slot unit F.component { initialInputs, validators, render: formless } (HE.input Formless)
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    Formless m a -> a <$ case m of
      F.Emit q -> eval q
      F.Submitted formOutputs -> do
        let user :: User
            user = F.unwrapOutputFields formOutputs
        H.liftEffect $ Console.logShow user
      F.Changed fstate -> do
        H.liftEffect $ Console.log $ show $ delete (SProxy :: SProxy "form") fstate

    Reset a -> a <$ do
      x <- H.query unit $ F.send Email (H.request TA.GetItems)
      -- If the reset succeeded, then print out the items.
      H.liftEffect $ Console.logShow $ fromMaybe [] x
      _ <- H.query unit $ F.send Email (H.action TA.Clear)
      _ <- H.query unit $ F.send Whiskey (H.action TA.Clear)
      _ <- H.query unit $ F.send Language (H.action TA.Clear)
      H.query unit F.resetAll_

    Typeahead slot (TA.SelectionsChanged new) a -> case slot of
      Email -> a <$ do
        H.query unit $ F.setValidate_ prx.email new

      Whiskey -> a <$ do
        _ <- H.query unit $ F.setValidate_ prx.whiskey new
        -- We'll clear the email field when a new whiskey is selected
        _ <- H.query unit $ F.reset_ prx.email
        H.query unit $ F.send Email (H.action TA.Clear)

      Language -> a <$ do
        H.query unit $ F.setValidate_ prx.language new
