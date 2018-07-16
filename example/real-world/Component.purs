module Example.RealWorld.Component where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.RealWorld.Data.Group (Group(..), _admin, _applications, _pixels, _secretKey1, _secretKey2, _whiskey)
import Example.RealWorld.Data.Options (Options(..), _metric)
import Example.RealWorld.Render.GroupForm as GroupForm
import Example.RealWorld.Render.Nav as Nav
import Example.RealWorld.Render.OptionsForm as OptionsForm
import Example.RealWorld.Spec.GroupForm (groupFormParser, groupFormSpec, groupFormValidation)
import Example.RealWorld.Spec.OptionsForm (optionsFormSpec, optionsFormValidation)
import Example.RealWorld.Types (ChildQuery, ChildSlot, GroupTASlot(..), Query(..), State, Tab(..))
import Formless as Formless
import Formless.Spec (unwrapOutput)
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
    , groupFormDirty: false
    , optionsFormErrors: 0
    , optionsFormDirty: false
    , group: Nothing
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
          , parser: groupFormParser
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
          , parser: Options <<< unwrapOutput
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

    -- We can reset both forms to their starting values by leveraging
    -- the `Reset` query from Formless
    Reset a -> do
      groupState <- H.query' CP.cp1 unit $ H.request Formless.ResetReply
      optionsState <- H.query' CP.cp2 unit $ H.request Formless.ResetReply
      H.modify_ \st -> st
        { groupFormErrors = maybe st.groupFormErrors _.errors groupState
        , groupFormDirty = maybe st.groupFormDirty _.dirty groupState
        , optionsFormErrors = maybe st.optionsFormErrors _.errors optionsState
        , optionsFormDirty = maybe st.optionsFormDirty _.dirty optionsState
        }
      pure a

    -- On submit, we need to make sure both forms are run. We
    -- can use the `SubmitReply` query to have submission return
    -- the result directly, rather than via independent messages.
    Submit a -> do
      mbGroupForm <- H.query' CP.cp1 unit $ H.request Formless.SubmitReply
      mbOptionsForm <- H.query' CP.cp2 unit $ H.request Formless.SubmitReply

      -- Here, we'll construct our new group from the two form outputs.
      case mbGroupForm, mbOptionsForm of
         Just g, Just v -> do
           H.modify_ _ { group = map (over Group (_ { options = v })) g }
         _, _ -> H.liftEffect (Console.error "Forms did not validate.")

      st <- H.get
      H.liftEffect $ Console.log $ show st.group
      pure a

    -----
    -- Group Form

    HandleGroupForm m a -> case m of
      Formless.Emit q -> eval q *> pure a
      -- We are manually querying Formless to get form submissions
      -- so we can safely ignore this.
      Formless.Submitted _ -> pure a

    HandleGroupTypeahead slot m a -> case m of
      TA.Emit q -> eval q *> pure a
      TA.SelectionsChanged s v -> do
        let v' = TA.unpackSelections v
        case slot of
          ApplicationsTypeahead -> do
            _ <- H.query' CP.cp1 unit $ Formless.handleChange _applications v'
            _ <- H.query' CP.cp1 unit $ Formless.handleBlur _applications
            pure a
          PixelsTypeahead -> do
            _ <- H.query' CP.cp1 unit $ Formless.handleChange _pixels v'
            _ <- H.query' CP.cp1 unit $ Formless.handleBlur _pixels
            pure a
          WhiskeyTypeahead -> case s of
            TA.ItemSelected x -> do
              _ <- H.query' CP.cp1 unit $ Formless.handleChange _whiskey (Just x)
              _ <- H.query' CP.cp1 unit $ Formless.handleBlur _whiskey
              pure a
            _ -> do
              _ <- H.query' CP.cp1 unit $ Formless.handleChange _whiskey Nothing
              _ <- H.query' CP.cp1 unit $ Formless.handleBlur _whiskey
              pure a
      TA.VisibilityChanged _ -> pure a
      TA.Searched _ -> pure a

    HandleAdminDropdown m a -> case m of
      Dropdown.ItemSelected x -> do
        _ <- H.query' CP.cp1 unit
          $ Formless.handleChange _admin (Just x)
        _ <- H.query' CP.cp1 unit
          $ Formless.handleBlur _admin

        -- Changing this field should also clear the secret keys
        _ <- H.query' CP.cp1 unit
          $ Formless.handleChange _secretKey1 ""
        _ <- H.query' CP.cp1 unit
          $ Formless.handleChange _secretKey2 ""
        pure a


    -----
    -- Options Form

    HandleOptionsForm m a -> case m of
      Formless.Emit q -> eval q *> pure a
      Formless.Submitted _ -> pure a

    HandleMetricDropdown m a -> case m of
      Dropdown.ItemSelected x -> do
        _ <- H.query' CP.cp2 unit $ Formless.handleChange _metric (Just x)
        _ <- H.query' CP.cp2 unit $ Formless.handleBlur _metric
        pure a
