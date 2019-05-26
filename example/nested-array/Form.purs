module Example.Nested.Form where

import Prelude

import Data.Array (filter, snoc, catMaybes)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.App.UI.Element (class_)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-----
-- Event form

-- Form types

type Event = { | EventRow F.OutputType }

newtype EventForm r f = EventForm (r (EventRow f))
derive instance newtypeEventForm :: Newtype (EventForm r f) _

type EventRow f =
  ( name :: f V.FieldError String String
  , location :: f V.FieldError String String
  , members :: f V.FieldError (Maybe (Array MemberInfo)) (Array MemberInfo)
  )

-- Form component types

type Slot =
  H.Slot (F.Query EventForm (Const Void) ChildSlots) Event

type State =
  ( formIds :: Array Int
  , nextId :: Int
  )

data Action
  = AddMemberForm
  | SubmitAll
  | HandleMemberForm Int MFMessage

type ChildSlots =
  ( memberForm :: MFSlot Int )

-- Form spec

eventComponent :: F.Component EventForm (Const Void) ChildSlots Unit Event Aff
eventComponent = F.component (const eventFormInput) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  eventFormInput :: F.Input EventForm State Aff
  eventFormInput =
    { validators: EventForm
        { name: V.minLength 3
        , location: V.minLength 3
        , members: F.hoistFn_ (fromMaybe [])
        }
    , initialInputs: Nothing
    , formIds: []
    , nextId: 0
    }

  handleAction = case _ of
    HandleMemberForm ix Destroy -> do
      H.modify_ \st -> st { formIds = filter (_ /= ix) st.formIds }
      eval $ F.set _members Nothing

    AddMemberForm ->
      H.modify_ \st -> st
        { nextId = st.nextId + 1, formIds = st.formIds `snoc` st.nextId }

    SubmitAll -> do
      st <- H.get
      res <- H.queryAll _memberForm $ H.request F.submitReply
      case map F.unwrapOutputFields $ catMaybes $ toUnfoldable $ M.values res of
        [] -> eval F.submit
        members -> eval (F.set _members (Just members)) *> eval F.submit

    where
    eval act = F.handleAction handleAction handleEvent act
    _members = SProxy :: _ "members"
    _memberForm = SProxy :: _ "memberForm"

  handleEvent = case _ of
    F.Submitted outputs ->
      H.raise (F.unwrapOutputFields outputs)
    _ -> pure unit

  render st =
    HH.div_
      [ HH.div
        [ class_ "field is-grouped" ]
        [ HH.div
            [ class_ "control" ]
            [ UI.button
                [ HE.onClick \_ -> Just $ F.injAction AddMemberForm ]
                [ HH.text "Add Member Form" ]
            ]
        , HH.div
            [ class_ "control" ]
            [ UI.buttonPrimary
                [ HE.onClick \_ -> Just $ F.injAction SubmitAll ]
                [ HH.text "Submit" ]
            ]
        ]
      , UI.input
          { label: "Event Name"
          , help: F.getResult _name st.form # UI.resultToHelp
              "Provide an event name"
          , placeholder: "My Event"
          }
          [ HP.value $ F.getInput _name st.form
          , HE.onValueInput $ Just <<< F.setValidate _name
          ]
      , UI.input
          { label: "Event Location"
          , help: F.getResult _location st.form # UI.resultToHelp
              "Provide an event location"
          , placeholder: "Los Angeles, CA"
          }
          [ HP.value $ F.getInput _location st.form
          , HE.onValueInput $ Just <<< F.setValidate _location
          ]
      , HH.div_
          (mkMemberForm <$> st.formIds)
      ]
    where
    mkMemberForm i = do
      let handler = Just <<< F.injAction <<< HandleMemberForm i
      HH.slot _memberForm i memberFormComponent unit handler

    _name = SProxy :: SProxy "name"
    _location = SProxy :: SProxy "location"
    _memberForm = SProxy :: SProxy "memberForm"


-----
-- Member form, nested inside
-----

-- Form types

type MemberInfo = { | MemberRow F.OutputType }

newtype MemberForm r f = MemberForm (r (MemberRow f))
derive instance newtypeMemberForm :: Newtype (MemberForm r f) _

type MemberRow f =
  ( name :: f V.FieldError String String
  , email :: f V.FieldError String V.Email
  , notes :: f Void String String
  )

-- Form component types

type MFSlot =
  H.Slot (F.Query' MemberForm) MFMessage

data MFAction = RemoveMe
data MFMessage = Destroy

-- Form spec

memberFormComponent :: F.Component MemberForm (Const Void) () Unit MFMessage Aff
memberFormComponent = F.component (const memberFormInput) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  }
  where
  memberFormInput :: F.Input' MemberForm Aff
  memberFormInput =
    { validators: MemberForm
        { name: V.minLength 5
        , email: V.emailFormat >>> V.emailIsUsed
        , notes: F.noValidation
        }
    , initialInputs: Nothing
    }

  handleAction = case _ of
    RemoveMe -> H.raise Destroy

  render st =
   UI.formContent_
     [ HH.div
         [ class_ "field" ]
         [ UI.buttonPrimary
             [ HE.onClick \_ -> Just $ F.injAction RemoveMe ]
             [ HH.text "Remove Me" ]
         ]
     , UI.input
         { label: "Member Name"
         , help: F.getResult _name st.form # UI.resultToHelp
             "Provide the registrant's name"
         , placeholder: "Dale Cooper"
         }
         [ HP.value $ F.getInput _name st.form
         , HE.onValueInput $ Just <<< F.setValidate _name
         ]
     , UI.input
         { label: "Member Email"
         , help: F.getResult _email st.form # UI.resultToHelp
             "Provide the registrant's email address"
         , placeholder: "dalecooper@fbi.gov"
         }
         [ HP.value $ F.getInput _email st.form
         , HE.onValueInput $ Just <<< F.setValidate _email
         ]
     , UI.input
         { label: "Additional Notes"
         , help: Right "Provide any additional notes you'd like."
         , placeholder: "Fond of Tibetan traditions"
         }
         [ HP.value $ F.getInput _notes st.form
         , HE.onValueInput $ Just <<< F.set _notes
         ]
    ]
    where
    _name = SProxy :: SProxy "name"
    _email = SProxy :: SProxy "email"
    _notes = SProxy :: SProxy "notes"
