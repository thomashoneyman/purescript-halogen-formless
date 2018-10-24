module Example.Nested.Component where

import Prelude

import Data.Array (filter, snoc)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Effect.Aff (Aff)
import Effect.Console as Console
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = HandleMemberForm Int (F.Message Query MemberForm) a
  | HandleEventForm (F.Message Query EventForm) a
  | AddMemberForm a
  | RemoveMemberForm Int a
  | SubmitAll a

type CQ = F.Query Query EventCQ EventCS EventForm Aff
type CS = Unit

type EventCQ = F.Query Query (Const Void) Void MemberForm Aff
type EventCS = Int

type State = { formIds :: Array Int, nextId :: Int }

component :: H.Component HH.HTML Query Unit Void Aff
component = H.parentComponent
  { initialState: const { formIds: [], nextId: 1 }
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: State -> H.ParentHTML Query CQ CS Aff
  render st =
    UI.section_
      [ UI.h1_ [ HH.text "Formless" ]
      , UI.h2_ [ HH.text "A form with a dynamic array of nested sub-forms." ]
      , UI.p_ $
          "It is possible to nest sub-forms within Formless like any other external component. This "
          <> "allows you to create an arbitrary number of forms within other forms while preserving the "
          <> "type safety provided by the Formless library. Try submitting the form with no sub-forms, and "
          <> "review the output in the console. Next, try adding one or more sub-forms and submitting them "
          <> "when valid or invalid."
      , HH.br_
      , HH.slot unit F.component
          { initialInputs: F.wrapInputFields { name: "", location: "", members: Nothing }
          , validators: eventFormValidation
          , render: renderEventForm st
          }
          (HE.input HandleEventForm)
      ]

  eval :: Query ~> H.ParentDSL State Query CQ CS Void Aff
  eval = case _ of

    -- We only care about queries raised out of the components, but no messages
    -- because we'll use `submitReply` later.
    HandleEventForm (F.Emit q) a -> eval q *> pure a
    HandleEventForm _ a -> pure a

    HandleMemberForm _ (F.Emit q) a -> eval q *> pure a
    HandleMemberForm _ _ a -> pure a

    SubmitAll a -> do
      st <- H.get

      -- First, we'll submit the member forms to set the top-level form
      memberForms <- for st.formIds \id -> H.query unit $ F.send id F.submitReply
      let members = (map <<< map) F.unwrapOutputFields $ traverse join memberForms

      -- Now, we'll use their outputs as the input to our top-level form component
      _ <- H.query unit $ F.set_ (SProxy :: SProxy "members") members

      -- and submit that form, accepting its output
      res <- H.query unit F.submitReply

      -- now we can review the results of submitting the form
      case join res of
        Nothing -> H.liftEffect $ Console.log "Forms are not valid"
        Just r -> H.liftEffect do
          Console.log "Valid!"
          Console.logShow $ F.unwrapOutputFields r

      pure a

    AddMemberForm a -> do
      H.modify_ \st -> st { nextId = st.nextId + 1, formIds = st.formIds `snoc` st.nextId }
      pure a

    RemoveMemberForm i a -> do
      H.modify_ \st -> st { formIds = filter (_ /= i) st.formIds }
      pure a

----------
-- Formless (Event Form)

type Event = Record (EventRow F.OutputType)

newtype EventForm r f = EventForm (r (EventRow f))
derive instance newtypeEventForm :: Newtype (EventForm r f) _

type EventRow f =
  ( name :: f V.FieldError String String
  , location :: f V.FieldError String String
  , members :: f V.FieldError (Maybe (Array MemberInfo)) (Array MemberInfo)
  )

eventFormValidation :: EventForm Record (F.Validation EventForm Aff)
eventFormValidation = EventForm
  { name: V.minLength 3
  , location: V.minLength 3
  , members: V.exists
  }

renderEventForm :: State -> F.State EventForm Aff -> F.HTML Query EventCQ EventCS EventForm Aff
renderEventForm st fs =
  HH.div_
    ( [ HH.div
        [ HP.class_ $ HH.ClassName "field is-grouped" ]
        [ HH.div
          [ HP.class_ $ HH.ClassName "control" ]
          [ UI.button
            [ HE.onClick $ HE.input_ $ F.raise $ H.action AddMemberForm ]
            [ HH.text "Add Member Form" ]
          ]
        , HH.div
          [ HP.class_ $ HH.ClassName "control" ]
          [ UI.buttonPrimary
            [ HE.onClick $ HE.input_ $ F.raise $ H.action SubmitAll ]
            [ HH.text "Submit" ]
          ]
        ]
      , UI.input
         { label: "Event Name"
         , help: UI.resultToHelp "Provide an event name" $ F.getResult _name fs.form
         , placeholder: "My Event"
         }
         [ HP.value $ F.getInput _name fs.form
         , HE.onValueInput $ HE.input $ F.setValidate _name
         ]
      , UI.input
         { label: "Event Location"
         , help: UI.resultToHelp "Provide an event location" $ F.getResult _location fs.form
         , placeholder: "Los Angeles, CA"
         }
         [ HP.value $ F.getInput _location fs.form
         , HE.onValueInput $ HE.input $ F.setValidate _location
         ]
      ]
      <> map mkMemberForm st.formIds
    )
  where
    mkMemberForm :: Int -> F.HTML Query EventCQ EventCS EventForm Aff
    mkMemberForm i =
      HH.slot i F.component
        { initialInputs: F.wrapInputFields { name: "", email: "", notes: "" }
        , validators: memberFormValidation
        , render: renderMemberForm i
        }
        (HE.input $ F.raise <<< H.action <<< HandleMemberForm i)

    _name = SProxy :: SProxy "name"
    _location = SProxy :: SProxy "location"



----------
-- Formless (Member Form)

type MemberInfo = Record (MemberRow F.OutputType)

newtype MemberForm r f = MemberForm (r (MemberRow f))
derive instance newtypeMemberForm :: Newtype (MemberForm r f) _

type MemberRow f =
  ( name :: f V.FieldError String String
  , email :: f V.FieldError String V.Email
  , notes :: f Void String String
  )

memberFormValidation :: MemberForm Record (F.Validation MemberForm Aff)
memberFormValidation = MemberForm
  { name: V.minLength 5
  , email: V.emailFormat >>> V.emailIsUsed
  , notes: F.hoistFn_ identity
  }

renderMemberForm :: Int -> F.State MemberForm Aff -> F.HTML Query (Const Void) Void MemberForm Aff
renderMemberForm i fs =
 UI.formContent_
 [ HH.div
   [ HP.class_ $ HH.ClassName "field" ]
   [ UI.buttonPrimary
     [ HE.onClick $ HE.input_ $ F.raise $ H.action $ RemoveMemberForm i ]
     [ HH.text "Remove Me" ]
   ]
 , UI.input
     { label: "Member Name"
     , help: UI.resultToHelp "Provide the registrant's name" $ F.getResult _name fs.form
     , placeholder: "Dale Cooper"
     }
     [ HP.value $ F.getInput _name fs.form
     , HE.onValueInput $ HE.input $ F.setValidate _name
     ]
 , UI.input
     { label: "Member Email"
     , help: UI.resultToHelp "Provide the registrant's email address" $ F.getResult _email fs.form
     , placeholder: "dalecooper@fbi.gov"
     }
     [ HP.value $ F.getInput _email fs.form
     , HE.onValueInput $ HE.input $ F.setValidate _email
     ]
 , UI.input
     { label: "Additional Notes"
     , help: Right "Provide any additional notes you'd like."
     , placeholder: "Fond of Tibetan traditions"
     }
     [ HP.value $ F.getInput _notes fs.form
     , HE.onValueInput $ HE.input $ F.set _notes
     ]
   ]
  where
    _name = SProxy :: SProxy "name"
    _email = SProxy :: SProxy "email"
    _notes = SProxy :: SProxy "notes"
