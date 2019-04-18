module Example.RealWorld.Component where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Element (class_)
import Example.App.UI.Element as UI
import Example.RealWorld.GroupForm as GF
import Example.RealWorld.OptionsForm as OF
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

-- Component types

data Action
  = HandleGroupForm GF.Message
  | HandleOptionsForm OF.Message
  | Select Tab
  | Reset
  | Submit

data Tab = GroupTab | OptionsTab
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab

-- We'll keep track of both form errors so we can show them in tabs. Our 
-- ultimate goal is to result in a Group.
type State =
  { focus :: Tab                 -- Which tab is the user on?
  , groupFormErrors :: Int       -- Count of the group form errors
  , groupFormDirty :: Boolean    -- Is the group form in a dirty state?
  , optionsFormErrors :: Int     -- Count of the options form errors
  , optionsFormDirty :: Boolean  -- Is the options form in a dirty state?
  , optionsEnabled :: Boolean    -- Is the options form enabled?
  , group :: Maybe GF.Group      -- Our ideal result type from form submission
  }

type ChildSlots = 
  ( groupForm :: GF.Slot Unit
  , optionsForm :: OF.Slot Unit
  )

_groupForm = SProxy :: SProxy "groupForm"
_optionsForm = SProxy :: SProxy "optionsForm"

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
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

  handleAction = case _ of
    HandleGroupForm { errors, dirty } ->
      H.modify_ _
        { groupFormErrors = errors
        , groupFormDirty = dirty
        }

    HandleOptionsForm { errors, dirty, enabled } ->
      H.modify_ _ 
        { optionsFormErrors = errors
        , optionsFormDirty = dirty
        , optionsEnabled = enabled
        }

    Select tab -> 
      H.modify_ _ { focus = tab }

    Reset -> do
      -- TODO: Obscure errors here
      {-

      -- we'll clear the group form using a new query
      _ <- H.query _groupForm unit (F.injQuery $ H.tell GF.ClearComponents)
      _ <- H.query _groupForm unit (F.asQuery F.resetAll)

      -- but we'll manually clear the options form dropdown through Formless
      _ <- F.sendQuery _optionsForm unit DD._dropdown unit DD.clear
      _ <- H.query _optionsForm unit $ F.asQuery F.resetAll

      -}
      pure unit

    -- On submit, we need to make sure both forms are run. We can use the 
    -- `SubmitReply` query to have submission return the result directly, 
    -- rather than via independent messages.
    Submit -> do
      mbGroupForm <- H.query _groupForm unit (H.request F.submitReply)
      mbOptionsForm <- H.query _optionsForm unit (H.request F.submitReply)
      -- Now, we'll construct our new group from the two form outputs.
      case mbGroupForm, mbOptionsForm of
         Just g, Just o -> do
           group <- traverse groupFormSubmit g
           let 
             options = map (OF.Options <<< F.unwrapOutputFields) o
           H.modify_ _ { group = map (over GF.Group (_ { options = options })) group }
         _, _ -> H.liftEffect (Console.error "Forms did not validate.")
      st <- H.get
      H.liftEffect $ Console.logShow st.group

  groupFormSubmit form = do
    -- This could be a server call or something else that is necessary
    -- to collect the information to complete your output type.
    groupId <- pure (GF.GroupId 10)
    pure 
      $ GF.Group
      $ Record.delete (SProxy :: SProxy "secretKey2")
      $ Record.rename (SProxy :: SProxy "secretKey1") (SProxy :: SProxy "secretKey")
      $ Record.insert (SProxy :: SProxy "id") groupId
      $ Record.insert (SProxy :: SProxy "options") Nothing
      $ F.unwrapRecord
      $ unwrap form

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render st =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A complex form inspired by real-world use cases." ]
      , UI.p_ 
          """
          This component demonstrates building a large form with complex rendering and validation requirements. Notice how both tabs end up unifying to a single output type after the two forms are combined, how various dropdowns determine the contents (and visibility) of other form elements, the assorted external components, and how validation for many fields depends on the values of other fields in the form.
          """ 
      , HH.br_
      , UI.p_
          """
          Next, review the source code. You'll notice that all of the complex types and state necessary to run this form can be generated from a pair of row types. All that's left for you to handle is to write the validation (with helper functions) and the render function.
          """
      , HH.br_
      , UI.grouped_
          [ UI.button
            [ HE.onClick \_ -> Just $ Select GroupTab ]
              [ UI.p_ $ "Group Form" <>
                  if st.groupFormErrors > 0
                    then " (" <> show st.groupFormErrors  <> ")"
                    else ""
              ]
          , UI.button
              [ HE.onClick \_ -> Just $ Select OptionsTab ]
              [ UI.p_ $ "Options Form" <>
                  if st.optionsFormErrors > 0
                    then " (" <> show st.optionsFormErrors  <> ")"
                    else ""
              ]
          , UI.buttonPrimary
              [ HE.onClick \_ -> Just Submit ]
              [ HH.text "Submit Form" ]
          , UI.button
              [ if st.groupFormDirty || st.optionsFormDirty
                  then HE.onClick \_ -> Just Reset
                  else HP.disabled true
              ]
              [ HH.text "Reset All" ]
          ]
      , HH.div
          [ class_ $ "is-hidden" # guard (st.focus == GroupTab) ]
          [ HH.slot _groupForm unit (F.component GF.spec) GF.input handleG ]
      , HH.div
          [ class_ $ "is-hidden" # guard (st.focus == OptionsTab) ]
          [ HH.slot _optionsForm unit (F.component OF.spec) OF.input handleO ]
      ]
    where
    handleG = Just <<< HandleGroupForm
    handleO = Just <<< HandleOptionsForm
