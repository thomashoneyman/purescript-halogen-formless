module Example.RealWorld.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Dropdown as DD
import Example.App.UI.Element (css)
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as TA
import Example.RealWorld.Data.Group as G
import Example.RealWorld.Data.Options as O
import Example.RealWorld.Render.GroupForm as GroupForm
import Example.RealWorld.Render.OptionsForm as OptionsForm
import Example.RealWorld.Spec.GroupForm (groupInputs, groupValidators, groupFormSubmit)
import Example.RealWorld.Spec.OptionsForm (optionsFormInputs, optionsFormValidators, defaultInputs)
import Example.RealWorld.Types (ChildQuery, ChildSlot, GroupTASlot(..), Query(..), State, Tab(..))
import Formless as F
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { focus: GroupTab
    , groupFormErrors: 0
    , groupFormDirty: false
    , optionsFormErrors: 0
    , optionsFormDirty: false
    , optionsEnabled: false
    , group: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A complex form inspired by real-world use cases." ]
    , UI.p_ $
        "This component demonstrates building a large form with complex rendering and validation "
        <> "requirements. Notice how both tabs end up unifying to a single output type after the "
        <> "two forms are combined, how various dropdowns determine the contents (and visibility) "
        <> "of other form elements, the assorted external components, and how validation for many "
        <> "fields depends on the values of other fields in the form."
    , HH.br_
    , UI.p_ $
        "Next, review the source code. You'll notice that all of the complex types and state necessary "
        <> "to run this form can be generated from a pair of row types. All that's left for you to handle "
        <> "is to write the validation (with helper functions) and the render function."
    , HH.br_
    , UI.grouped_
      [ UI.button
        [ HE.onClick $ HE.input_ $ Select GroupTab ]
        [ UI.p_ $ "Group Form" <>
            if st.groupFormErrors > 0
              then " (" <> show st.groupFormErrors  <> ")"
              else ""
        ]
      , UI.button
        [ HE.onClick $ HE.input_ $ Select OptionsTab ]
        [ UI.p_ $ "Options Form" <>
            if st.optionsFormErrors > 0
              then " (" <> show st.optionsFormErrors  <> ")"
              else ""
        ]
      , UI.buttonPrimary
        [ HE.onClick $ HE.input_ Submit ]
        [ HH.text "Submit Form" ]
      , UI.button
        [ if st.groupFormDirty || st.optionsFormDirty
            then HE.onClick $ HE.input_ Reset
            else HP.disabled true
        ]
        [ HH.text "Reset All" ]
      ]
    , HH.div
      [ if st.focus == GroupTab then css "" else css "is-hidden" ]
      [ HH.slot' CP.cp1 unit F.component
          { initialInputs: groupInputs
          , validators: groupValidators
          , render: GroupForm.render
          } (HE.input GroupForm)
      ]
    , HH.div
      [ if st.focus == OptionsTab then css "" else css "is-hidden" ]
      [ HH.slot' CP.cp2 unit F.component
          { initialInputs: defaultInputs
          , validators: optionsFormValidators
          --  , submitter: pure <<< O.Options <<< F.unwrapOutputFields
          , render: OptionsForm.render
          } (HE.input OptionsForm)
      ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    Select tab a -> do
      H.modify_ _ { focus = tab }
      pure a

    Reset a -> do
      -- To send a query through to a child component when Formless has multiple, use send'
      _ <- H.query' CP.cp1 unit $ F.send' CP.cp1 Applications (H.action TA.Clear)
      _ <- H.query' CP.cp1 unit $ F.send' CP.cp1 Pixels (H.action TA.Clear)
      _ <- H.query' CP.cp1 unit $ F.send' CP.cp2 unit (H.action TA.Clear)
      _ <- H.query' CP.cp1 unit $ F.send' CP.cp3 unit (H.action DD.Clear)
      -- If there is only one child type, use Send
      _ <- H.query' CP.cp2 unit $ F.send unit (H.action DD.Clear)
      _ <- H.query' CP.cp1 unit F.resetAll_
      _ <- H.query' CP.cp2 unit F.resetAll_
      pure a

    -- On submit, we need to make sure both forms are run. We
    -- can use the `SubmitReply` query to have submission return
    -- the result directly, rather than via independent messages.
    Submit a -> do
      mbGroupForm <- H.query' CP.cp1 unit $ H.request F.SubmitReply
      mbOptionsForm <- H.query' CP.cp2 unit $ H.request F.SubmitReply
      -- Here, we'll construct our new group from the two form outputs.
      case mbGroupForm, mbOptionsForm of
         Just g, Just o -> do
           -- We can run our monadic submission function here
           group :: Maybe G.Group <- traverse groupFormSubmit g
           -- We can unwrap our options purely
           let options :: Maybe O.Options
               options = O.Options <<< F.unwrapOutputFields <$> o
           H.modify_ _ { group = map (over G.Group (_ { options = options })) group }
         _, _ -> H.liftEffect (Console.error "Forms did not validate.")
      st <- H.get
      H.liftEffect $ Console.log $ show st.group
      pure a

    -----
    -- Group Form

    GroupForm m a -> case m of
      F.Emit q -> eval q $> a
      F.Submitted _ -> pure a
      F.Changed fstate -> do
        H.modify_ \st -> st { groupFormErrors = fstate.errors, groupFormDirty = fstate.dirty }
        pure a

    TASingle (TA.SelectionsChanged new) a -> a <$ do
      H.query' CP.cp1 unit $ F.setValidate_ G.prx.whiskey new

    TAMulti slot (TA.SelectionsChanged new) a -> a <$ case slot of
      Applications ->
        H.query' CP.cp1 unit $ F.setValidate_ G.prx.applications new
      Pixels ->
        H.query' CP.cp1 unit $ F.setValidate_ G.prx.pixels new

    AdminDropdown m a -> a <$ do
      _ <- H.query' CP.cp1 unit $ F.reset_ G.prx.secretKey1
      _ <- H.query' CP.cp1 unit $ F.reset_ G.prx.secretKey2
      case m of
        DD.Selected x -> do
          H.query' CP.cp1 unit $ F.setValidate_ G.prx.admin (Just x)
        DD.Cleared -> do
          H.query' CP.cp1 unit $ F.setValidate_ G.prx.admin Nothing

    -----
    -- Options Form

    OptionsForm m a -> case m of
      F.Emit q -> eval q $> a
      F.Submitted _ -> pure a
      F.Changed fstate -> do
        st <- H.get
        st' <- H.modify _
          { optionsFormErrors = fstate.errors
          , optionsFormDirty = fstate.dirty
          , optionsEnabled = F.getInput O.prx.enable fstate.form
          }

        -- The generated spec will set enabled to false, but we'll want it to be true before
        -- sending a new spec in to the component.
        when (st.optionsEnabled /= st'.optionsEnabled) do
          case st'.optionsEnabled of
            true -> do
              let spec' = O.OptionsForm $ _ { enable = F.InputField true } $ unwrap optionsFormInputs
              void $ H.query' CP.cp2 unit $ F.loadForm_ spec'
            _ -> do
              void $ H.query' CP.cp2 unit $ F.loadForm_ defaultInputs
        pure a

    MetricDropdown m a -> a <$ case m of
      DD.Selected x -> do
        H.query' CP.cp2 unit $ F.setValidate_ O.prx.metric (Just x)
      DD.Cleared -> do
        H.query' CP.cp2 unit $ F.setValidate_ O.prx.metric Nothing
