module Example.RealWorld.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.RealWorld.Data.Group (_admin, _applications, _pixels, _whiskey)
import Example.RealWorld.Data.Options (_metric)
import Example.RealWorld.Render.GroupForm as GroupForm
import Example.RealWorld.Render.Nav as Nav
import Example.RealWorld.Render.OptionsForm as OptionsForm
import Example.RealWorld.Spec.GroupForm (groupFormSpec, groupFormValidation)
import Example.RealWorld.Spec.OptionsForm (optionsFormSpec, optionsFormValidation)
import Example.RealWorld.Types
  (ChildQuery, ChildSlot, GroupTASlot(..), Query(..), State, Tab(..))
import Formless as Formless
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Format as Format
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.Components.Typeahead as TA
import Ocelot.HTML.Properties (css)

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
    { focus: GroupFormTab
    , groupFormErrors: 0
    , optionsFormErrors: 0
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.div
    [ css "p-12 w-full container" ]
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
    , HH.div
      [ if st.focus == GroupFormTab then css "" else css "hidden" ]
      [ HH.slot'
          CP.cp1
          unit
          Formless.component
          { formSpec: groupFormSpec
          , validator: pure <$> groupFormValidation
          , render: GroupForm.render
          }
          (HE.input HandleGroupForm)
      ]
    , HH.div
      [ if st.focus == OptionsFormTab then css "" else css "hidden" ]
      [ HH.slot'
          CP.cp2
          unit
          Formless.component
          { formSpec: optionsFormSpec
          , validator: pure <$> optionsFormValidation
          , render: OptionsForm.render
          }
          (HE.input HandleOptionsForm)
      ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of

  	-----
  	-- Parent

    Select tab a -> do
      H.modify_ _ { focus = tab }
      pure a

    -- On submit, we need to make sure both forms are run. We
    -- can use the `SubmitReply` query to have submission return
    -- the result directly, rather than via independent messages.
    Submit a -> do
      groupForm <- H.query' CP.cp1 unit $ H.request Formless.SubmitReply
      optionsForm <- H.query' CP.cp2 unit $ H.request Formless.SubmitReply
      H.liftEffect case groupForm, optionsForm of
        Just (Left _), Just (Left _) -> do
          Console.error "Neither form validated successfully."

        Just (Left _), Just (Right _) -> do
          Console.warn "Only the options form validated successfully."

        Just (Right _), Just (Left _) -> do
          Console.warn "Only the group form validated successfully."

        Just (Right _), Just (Right _) -> do
          Console.info "Both forms validated successfully."

        Nothing, Just v -> do
          Console.error "The group form doesn't exist at that slot."

        Just v, Nothing -> do
          Console.error "The options form doesn't exist at that slot."

        Nothing, Nothing -> do
          Console.error "Something went wrong with the both forms."

      pure a

    -----
    -- Group Form

    HandleGroupForm m a -> case m of
      Formless.Emit q -> eval q *> pure a
  		-- We are manually querying Formless to get form submissions
  		-- so we can safely ignore this.
      Formless.Submitted _ -> pure a

      -- We don't care about the failed form result, but we do want
      -- to collect errors on validation.
      Formless.Validated _ errors -> do
        H.modify_ _ { groupFormErrors = errors }
        pure a

    HandleGroupTypeahead slot m a -> case m of
      TA.Emit q -> eval q *> pure a
      TA.SelectionsChanged s v -> do
        let v' = TA.unpackSelections v
        case s of
          TA.ItemSelected x -> case slot of
            ApplicationsTypeahead -> do
              _ <- H.query' CP.cp1 unit $ Formless.handleChange _applications v'
              _ <- H.query' CP.cp1 unit $ Formless.handleBlur _applications
              pure a
            PixelsTypeahead -> do
              _ <- H.query' CP.cp1 unit $ Formless.handleChange _pixels v'
              _ <- H.query' CP.cp1 unit $ Formless.handleBlur _pixels
              pure a
            WhiskeyTypeahead -> do
              _ <- H.query' CP.cp1 unit $ Formless.handleChange _whiskey (Just x)
              _ <- H.query' CP.cp1 unit $ Formless.handleBlur _whiskey
              pure a
          _ -> pure a
      TA.VisibilityChanged _ -> pure a
      TA.Searched _ -> pure a

    HandleAdminDropdown m a -> case m of
      Dropdown.ItemSelected x -> do
        _ <- H.query' CP.cp1 unit $ Formless.handleChange _admin (Just x)
        _ <- H.query' CP.cp1 unit $ Formless.handleBlur _admin
        pure a


    -----
    -- Options Form

    HandleOptionsForm m a -> case m of
      Formless.Emit q -> eval q *> pure a
      Formless.Submitted _ -> pure a
      Formless.Validated _ errors -> do
        H.modify_ _ { optionsFormErrors = errors }
        pure a

    HandleMetricDropdown m a -> case m of
      Dropdown.ItemSelected x -> do
        _ <- H.query' CP.cp2 unit $ Formless.handleChange _metric (Just x)
        _ <- H.query' CP.cp2 unit $ Formless.handleBlur _metric
        pure a
