module App.Form where

import Prelude

import Data.Array (reverse)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as Formless
import Formless.Spec (FormSpec(..), formSpecToInputField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleFormless (Formless.Message Query) a

-- | Yea, I know
type State = Unit

-- | Form inputs are expected to have this particular shape and rely
-- | on the `InputField` type from Formless.
newtype Form f = Form
  { name :: f String String String
  , email :: f String String String
  }
derive instance newtypeForm :: Newtype (Form f) _

form :: Form FormSpec
form = Form
  { name: FormSpec
      { input: ""
      , validator: const (Left "Not implemented.")
      }
  , email: FormSpec
      { input: ""
      , validator: const (Left "Not implemented.")
      }
  }


-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = Formless.Query Query FCQ FCS Form Aff
type ChildSlot = Unit

-- | Now we can create our form component. We'll essentially write a render
-- | function for Formless and pass it in.
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
  render _ =
    HH.div_
      [ HH.h2_
          [ HH.text "Formless" ]
      , HH.slot
          unit
          Formless.component
          { form: formSpecToInputField form
          , render: renderFormless }
          ( HE.input HandleFormless )
      , HH.hr_
      , HH.p_
          [ HH.text "Thanks for working on this form." ]
      ]

  eval
    :: Query
    ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    HandleFormless _ a -> pure a


----------
-- Formless

-- | Your parent component must provide a ChildQuery type to Formless
-- | that represents what sorts of children it can have, and an accompanying
-- | child slot type. In this case we'll provide no child query or child slot.
-- |
-- | FCQ: Formless ChildQuery
-- | FCS: Formless ChildSlot
type FCQ = Const Void
type FCS = Unit

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
renderFormless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
renderFormless state =
  HH.div_
    [ HH.h3_
      [ HH.text "Fill out the form:" ]
    , renderName state
    , renderEmail state
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderName state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = SProxy :: SProxy "name"
      label = "Name"
   in
      HH.div_
        ( [ HH.label_
            [ HH.text label ]
          , HH.br_
          , HH.code_
            [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
          , HH.br_
          , HH.input
            [ HP.value field.input
            , HE.onBlur $ HE.input_ $ Formless.HandleBlur (Formless.handleBlur sym)
            , HE.onValueInput $ HE.input \str -> Formless.HandleChange (Formless.handleChange sym str)
            ]
          , HH.br_
          , if field.touched
              then HH.text "-- changed since form initialization --"
              else HH.text ""
          , HH.br_
          ]
        <>
        case field.result of
          Nothing -> [ HH.text "" ]
          Just (Left err) -> [ HH.text err ]
          Just (Right _) -> [ HH.text "" ]
        <>
        [ HH.br_, HH.br_ ]
        )

renderEmail :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderEmail state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = SProxy :: SProxy "email"
      label = "Email"
   in
      HH.div_
        ( [ HH.label_
            [ HH.text label ]
          , HH.br_
          , HH.code_
            [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
          , HH.br_
          , HH.input
            [ HP.value field.input
            , HE.onBlur $ HE.input_ $ Formless.HandleBlur (Formless.handleBlur sym)
            , HE.onValueInput $ HE.input \str -> Formless.HandleChange (Formless.handleChange sym str)
            ]
          , HH.br_
          , if field.touched
              then HH.text "-- changed since form initialization --"
              else HH.text ""
          , HH.br_
          ]
        <>
        case field.result of
          Nothing -> [ HH.text "" ]
          Just (Left err) -> [ HH.text err ]
          Just (Right _) -> [ HH.text "" ]
        <>
        [ HH.br_, HH.br_ ]
        )
