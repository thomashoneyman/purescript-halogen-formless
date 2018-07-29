module Example.RealWorld.Render.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.App.UI.Dropdown as Dropdown
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as Typeahead
import Example.RealWorld.Data.Group (Admin(..), GroupId(..))
import Example.RealWorld.Data.Group as G
import Example.RealWorld.Types (GroupCQ, GroupCS, GroupTASlot(..), Query(..))
import Formless as F
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- | A convenience synonym for the group Formless state
type FormlessState
  = F.State G.GroupForm G.Group Aff

-- | A convenience synonym for the group Formless HTML type
type FormlessHTML
  = F.HTML Query GroupCQ GroupCS G.GroupForm G.Group Aff

-- | The form, grouped by sections.
render :: FormlessState -> FormlessHTML
render state =
  UI.formContent_
  [ renderName state
  , renderAdmin state
  , renderSecretKey1 state
  , renderSecretKey2 state
  , renderApplications state
  , renderPixels state
  , renderWhiskey state
  ]

-----
-- Built fields

renderName :: FormlessState -> FormlessHTML
renderName =
  UI.formlessField UI.input
  { label: "Name"
  , help: "Give the group a name."
  , placeholder: "January Analytics Seminar"
  , sym: G._name
  }

renderSecretKey1 :: FormlessState -> FormlessHTML
renderSecretKey1 =
  UI.formlessField UI.input
  { label: "Secret Key 1"
  , help: "Provide a secret identifier for the group."
  , placeholder: "iasncat3ihba/0"
  , sym: G._secretKey1
  }

renderSecretKey2 :: FormlessState -> FormlessHTML
renderSecretKey2 =
  UI.formlessField UI.input
  { label: "Secret Key 2"
  , help: "Confirm the secret identifier for the group."
  , placeholder: "iasncat3ihba/0"
  , sym: G._secretKey2
  }

renderAdmin :: FormlessState -> FormlessHTML
renderAdmin state =
  UI.field
    { label: "Administrator"
    , help: UI.resultToHelp "Choose an administrator for the account" (F.getResult G._admin state.form)
    }
    [ HH.slot' CP.cp3 unit Dropdown.component
        { items, placeholder: "Choose an admin" }
        ( HE.input $ F.Raise <<< H.action <<< AdminDropdown )
    ]
  where
    items =
      [ Admin { id: Nothing }
      , Admin { id: Just $ GroupId 10 }
      , Admin { id: Just $ GroupId 15 }
      , Admin { id: Just $ GroupId 20 }
      , Admin { id: Just $ GroupId 25 }
      , Admin { id: Just $ GroupId 30 }
      , Admin { id: Just $ GroupId 35 }
      ]

renderWhiskey :: FormlessState -> FormlessHTML
renderWhiskey state =
  UI.field
    { label: "Whiskey"
    , help: UI.resultToHelp "Choose a whiskey to be awarded" (F.getResult G._whiskey state.form)
    }
    [ HH.slot' CP.cp2 unit Typeahead.single
      { placeholder: "Choose a whiskey"
      , items:
        [ "Laphroiag 10"
        , "Lagavulin 12"
        , "Lagavulin 16"
        , "Oban 16"
        , "Kilchoman Blue Label"
        ]
      }
      ( HE.input $ F.Raise <<< H.action <<< TASingle )
    ]

renderPixels :: FormlessState -> FormlessHTML
renderPixels state =
  UI.field
    { label: "Tracking Pixels"
    , help: UI.resultToHelp "Choose a pixel to track" (F.getResult G._pixels state.form)
    }
    [ HH.slot' CP.cp1 Pixels Typeahead.multi
      { placeholder: "Search pixels"
      , items:
        [ "My favorite pixel"
        , "Your favorite pixel"
        , "Application main pixel"
        , "A pixel for you is a pixel for me"
        ]
      }
      ( HE.input $ F.Raise <<< H.action <<< TAMulti Pixels )
    ]

renderApplications :: FormlessState -> FormlessHTML
renderApplications state =
  UI.field
    { label: "Application Targets"
    , help: UI.resultToHelp "Applications are available in several sizes" (F.getResult G._applications state.form)
    }
    [ HH.slot' CP.cp1 Applications Typeahead.multi
      { placeholder: "Search one or more applications"
      , items: [ "Facebook", "Google", "Twitter", "Pinterest" ]
      }
      ( HE.input $ F.Raise <<< H.action <<< TAMulti Applications )
    ]
