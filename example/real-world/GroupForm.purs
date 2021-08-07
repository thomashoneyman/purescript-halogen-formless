module Example.RealWorld.GroupForm where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Example.App.UI.Dropdown as DD
import Example.App.UI.Element (class_)
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as TA
import Example.App.Validation (class ToText, FieldError)
import Example.App.Validation as V
import Example.RealWorld.OptionsForm as OF
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Select as Select
import Type.Proxy (Proxy(..))

-- Supporting types

newtype GroupId = GroupId Int
derive instance newtypeGroupId :: Newtype GroupId _
derive newtype instance eqGroupId :: Eq GroupId
derive newtype instance showGroupId :: Show GroupId

newtype Admin = Admin { id :: Maybe GroupId }
derive instance newtypeAdmin :: Newtype Admin _
derive newtype instance eqAdmin :: Eq Admin
derive newtype instance showAdmin :: Show Admin

instance toTextAdmin :: ToText Admin where
  toText (Admin { id }) = case id of
    Just (GroupId n) -> "Administrator " <> show n
    Nothing -> "None"

-- As far as our application is concerned this is the type that matters. Most
-- fields are directly out of our form and we could have used `F.OutputType` to
-- recover them. To be explicit, though, we'll copy them here.
--
-- The extra fields `id` and `secretKey` will come from the server after
-- submission, and the `options` field will come from our sub-form.
newtype Group = Group
  { name :: String
  , admin :: Admin
  , applications :: Array String
  , pixels :: Array String
  , whiskey :: String
  , id :: GroupId
  , secretKey :: String
  , options :: Maybe OF.Options
  }
derive instance newtypeGroup :: Newtype Group _
derive newtype instance eqGroup :: Eq Group
derive newtype instance showGroup :: Show Group

data Tab = GroupTab | OptionsTab
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab

-- Form types

newtype GroupForm (r :: Row Type -> Type) f = GroupForm (r (GroupFormRow f))
derive instance newtypeGroupForm :: Newtype (GroupForm r f) _

type GroupFormRow :: (Type -> Type -> Type -> Type) -> Row Type
type GroupFormRow f =
  ( name :: f FieldError String String
  , admin :: f FieldError (Maybe Admin) Admin
  , applications :: f FieldError (Array String) (Array String)
  , pixels :: f FieldError (Array String) (Array String)
  , whiskey :: f FieldError (Maybe String) String
  , secretKey1 :: f FieldError String String
  , secretKey2 :: f FieldError String String
  )

-- Form component types

type Slot =
  H.Slot (F.Query GroupForm (Const Void) ChildSlots) Group

_groupForm = Proxy :: Proxy "groupForm"

type State =
  ( selectedTab :: Tab -- which tab the user is viewing
  , optionsErrors :: Int -- count of errors in the options form
  , optionsDirty :: Boolean -- whether the options form has been edited
  )

data Action
  = Select Tab
  | UpdateKey1 String
  | UpdateKey2 String
  | HandleDropdown (DD.Message Admin)
  | HandleTASingle (TA.Message Maybe String)
  | HandleTAMulti TASlot (TA.Message Array String)
  | HandleOptionsForm OF.Message
  | ResetForm

type ChildSlots =
  ( dropdown :: DD.Slot Admin Unit
  , typeaheadSingle :: TA.Slot Maybe String Unit
  , typeaheadMulti :: TA.Slot Array String TASlot
  , optionsForm :: OF.Slot Unit
  )

data TASlot = Applications | Pixels
derive instance eqTASlot :: Eq TASlot
derive instance ordTASlot :: Ord TASlot

-- Form spec

prx :: F.SProxies GroupForm
prx = F.mkSProxies (Proxy :: Proxy GroupForm)

component :: F.Component GroupForm (Const Void) ChildSlots Unit Group Aff
component = F.component (const input) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  input :: F.Input GroupForm State Aff
  input =
    { validators: GroupForm
        { name: V.nonEmptyStr
        , admin: V.exists
        , applications: V.nonEmptyArray
        , pixels: V.nonEmptyArray
        , whiskey: V.exists
        , secretKey1: V.nonEmptyStr >>> V.minLength 5 >>> equalsSecretKey2
        , secretKey2: V.nonEmptyStr >>> V.minLength 5 >>> equalsSecretKey1
        }
    , initialInputs: Nothing
    , selectedTab: GroupTab
    , optionsErrors: 0
    , optionsDirty: false
    }
    where
    equalsSecretKey2 = F.hoistFnE \form secretKey1 -> do
      let secretKey2 = F.getInput prx.secretKey2 form
      if secretKey1 == secretKey2 then Right secretKey1
      else Left $ V.NotEqual secretKey2 secretKey1

    equalsSecretKey1 = F.hoistFnE \form secretKey2 -> do
      let secretKey1 = F.getInput prx.secretKey1 form
      if secretKey2 == secretKey1 then Right secretKey2
      else Left $ V.NotEqual secretKey1 secretKey2

  handleEvent = case _ of
    F.Submitted form -> do
      -- first, we'll submit the options form
      mbOptionsForm <- H.request OF._optionsForm unit F.submitReply
      let options = map (OF.Options <<< F.unwrapOutputFields) (join mbOptionsForm)
      -- next, we'll fetch a new group id (in the real world this might be a server call)
      groupId <- pure $ GroupId 10
      -- then, we'll produce a new Group by transforming our form outputs and raise it
      -- as a message.
      H.raise
        $ Group
        $ Record.delete (Proxy :: _ "secretKey2")
        $ Record.rename (Proxy :: _ "secretKey1") (Proxy :: _ "secretKey")
        $ Record.insert (Proxy :: _ "id") groupId
        $ Record.insert (Proxy :: _ "options") options
        $ F.unwrapOutputFields form
    _ -> pure unit

  handleAction = case _ of
    Select tab ->
      H.modify_ _ { selectedTab = tab }

    ResetForm -> do
      -- first, we'll reset this form's components
      _ <- H.query TA._typeaheadMulti Applications TA.clear
      _ <- H.query TA._typeaheadMulti Pixels TA.clear
      _ <- H.query TA._typeaheadSingle unit TA.clear
      _ <- H.query DD._dropdown unit DD.clear
      -- then, we'll reset the options form's components
      _ <- F.sendQuery OF._optionsForm unit DD._dropdown unit DD.clear
      -- and then we'll reset the forms
      _ <- H.query OF._optionsForm unit (F.asQuery F.resetAll)
      eval F.resetAll

    HandleOptionsForm { errors, dirty } ->
      H.modify_ _
        { optionsErrors = errors
        , optionsDirty = dirty
        }

    UpdateKey1 key -> do
      eval $ F.setValidate prx.secretKey1 key
      eval $ F.validate prx.secretKey2

    UpdateKey2 key -> do
      eval $ F.setValidate prx.secretKey2 key
      eval $ F.validate prx.secretKey1

    HandleDropdown dropdownMessage -> do
      eval $ F.reset prx.secretKey1
      eval $ F.reset prx.secretKey2
      case dropdownMessage of
        DD.Selected admin ->
          eval $ F.setValidate prx.admin (Just admin)
        DD.Cleared ->
          eval $ F.setValidate prx.admin Nothing

    HandleTASingle (TA.SelectionsChanged new) ->
      eval $ F.setValidate prx.whiskey new

    HandleTAMulti slot (TA.SelectionsChanged new) -> case slot of
      Applications -> eval $ F.setValidate prx.applications new
      Pixels -> eval $ F.setValidate prx.pixels new

    where
    eval act = F.handleAction handleAction handleEvent act

  render st@{ form } =
    HH.div_
      [ UI.grouped_
          [ UI.button
              [ HE.onClick \_ -> F.injAction $ Select GroupTab ]
              [ UI.p_ $ "Group Form" <>
                  if st.errors > 0 then " (" <> show st.errors <> ")"
                  else ""
              ]
          , UI.button
              [ HE.onClick \_ -> F.injAction $ Select OptionsTab ]
              [ UI.p_ $ "Options Form" <>
                  if st.optionsErrors > 0 then " (" <> show st.optionsErrors <> ")"
                  else ""
              ]
          , UI.buttonPrimary
              [ HE.onClick \_ -> F.submit ]
              [ HH.text "Submit Form" ]
          , UI.button
              [ if st.dirty || st.optionsDirty then HE.onClick \_ -> F.injAction ResetForm
                else HP.disabled true
              ]
              [ HH.text "Reset All" ]
          ]
      , HH.div
          [ class_ $ "is-hidden" # guard (st.selectedTab /= GroupTab) ]
          [ UI.formContent_
              [ renderName
              , renderAdmin
              , renderSecretKey1
              , renderSecretKey2
              , renderApplications
              , renderPixels
              , renderWhiskey
              ]
          ]
      , HH.div
          [ class_ $ "is-hidden" # guard (st.selectedTab /= OptionsTab) ]
          [ HH.slot OF._optionsForm unit OF.component unit handleOF ]
      ]
    where
    handleOF = F.injAction <<< HandleOptionsForm

    renderName = st # UI.formlessField UI.input
      { label: "Name"
      , help: "Give the group a name."
      , placeholder: "January Analytics Seminar"
      , sym: prx.name
      }

    renderSecretKey1 = UI.input
      { label: "Secret Key 1"
      , help: F.getResult prx.secretKey1 form # UI.resultToHelp
          "Provide a secret identifier for the group"
      , placeholder: "ia30<>Psncdi3b#$<0423"
      }
      [ HP.value $ F.getInput prx.secretKey1 form
      , HE.onValueInput $ F.injAction <<< UpdateKey1
      ]

    renderSecretKey2 = UI.input
      { label: "Secret Key 1"
      , help: F.getResult prx.secretKey2 form # UI.resultToHelp
          "Confirm the secret identifier for the group"
      , placeholder: "ia30<>Psncdi3b#$<0423"
      }
      [ HP.value $ F.getInput prx.secretKey2 form
      , HE.onValueInput $ F.injAction <<< UpdateKey2
      ]

    renderAdmin = UI.field
      { label: "Administrator"
      , help: F.getResult prx.admin form # UI.resultToHelp
          "Choose an administrator for the account"
      }
      [ HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler ]
      where
      handler = F.injAction <<< HandleDropdown
      ddInput =
        { placeholder: "Choose an admin"
        , items: map (Admin <<< { id: _ })
            [ Nothing
            , Just $ GroupId 10
            , Just $ GroupId 15
            , Just $ GroupId 20
            , Just $ GroupId 25
            , Just $ GroupId 30
            , Just $ GroupId 35
            ]
        }

    renderWhiskey = UI.field
      { label: "Whiskey"
      , help: F.getResult prx.whiskey form # UI.resultToHelp
          "Choose a whiskey to be awarded"
      }
      [ HH.slot TA._typeaheadSingle unit (Select.component TA.input TA.single) taInput handler ]
      where
      handler = F.injAction <<< HandleTASingle
      taInput =
        { placeholder: "Choose a whiskey"
        , items:
            [ "Laphroiag 10"
            , "Lagavulin 12"
            , "Lagavulin 16"
            , "Oban 16"
            , "Kilchoman Blue Label"
            ]
        }

    renderPixels = UI.field
      { label: "Tracking Pixels"
      , help: F.getResult prx.pixels form # UI.resultToHelp "Choose a pixel to track"
      }
      [ HH.slot TA._typeaheadMulti Pixels selectComponent taInput handler ]
      where
      selectComponent = Select.component TA.input TA.multi
      handler = F.injAction <<< HandleTAMulti Pixels
      taInput =
        { placeholder: "Search pixels"
        , items:
            [ "My favorite pixel"
            , "Your favorite pixel"
            , "Application main pixel"
            , "A pixel for you is a pixel for me"
            ]
        }

    renderApplications = UI.field
      { label: "Application Targets"
      , help: F.getResult prx.applications form # UI.resultToHelp
          "Applications are available in several sizes."
      }
      [ HH.slot TA._typeaheadMulti Applications selectComponent taInput handler ]
      where
      selectComponent = Select.component TA.input TA.multi
      handler = F.injAction <<< HandleTAMulti Applications
      taInput =
        { placeholder: "Search one or more applications"
        , items: [ "Facebook", "Google", "Twitter", "Pinterest" ]
        }
