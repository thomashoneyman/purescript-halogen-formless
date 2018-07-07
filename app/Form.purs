module App.Form where

import Prelude

import Data.Array (reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as Formless
import Formless.Spec (FormSpec(..), formSpecToInputFields)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Record as Record

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleFormless (Formless.Message Query) a
  | HandleTypeahead (TA.Message Query String) a

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
      , validator: \str ->
          if String.length str < 3
            then Left "Must be 3 characters or more."
            else Right str
      }
  , email: FormSpec
      { input: ""
      , validator: \str ->
          if String.contains (String.Pattern "@") str
            then Left "Email addresses can't contain the '@' symbol."
            else Right str
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

  css :: ∀ p i. String -> H.IProp ( "class" :: String | p ) i
  css = HP.class_ <<< HH.ClassName

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render _ =
    HH.div
      [ css "p-6 flex flex-1" ]
      [ HH.div
        [ css "flex-1 mx-10 mt-10" ]
        [ Format.heading_
          [ HH.text "Formless" ]
        , HH.slot
            unit
            Formless.component
            { formSpec: formSpecToInputFields form -- TODO: should be able to just pass the spec in...
            , render: renderFormless }
            ( HE.input HandleFormless )
        ]
      ]

  eval
    :: Query
    ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    -- Always have to handle the `Emit` case
    HandleFormless m a -> case m of
      Formless.Emit q -> eval q *> pure a
      _ -> pure a

    -- Always have to handle the `Emit` case. Because we aren't hooking directly
    -- into the component's effects, we'll have to use its output to manage validation
    -- and change events.
    HandleTypeahead m a -> case m of
      TA.Emit q -> eval q *> pure a
      TA.SelectionsChanged _ _ -> do
        _ <- H.query unit $ H.action $ Formless.HandleChange $ Formless.handleChange (SProxy :: SProxy "email") "a@a.com"
        _ <- H.query unit $ H.action $ Formless.HandleBlur (Formless.handleBlur (SProxy :: SProxy "email"))
        pure a
      TA.VisibilityChanged _ -> do
        _ <- H.query unit $ H.action $ Formless.HandleBlur (Formless.handleBlur (SProxy :: SProxy "email"))
        pure a
      _ -> pure a


----------
-- Formless

-- | Your parent component must provide a ChildQuery type to Formless
-- | that represents what sorts of children it can have, and an accompanying
-- | child slot type. In this case we'll provide no child query or child slot.
-- |
-- | FCQ: Formless ChildQuery
-- | FCS: Formless ChildSlot
type FCQ = TA.Query Query String String Aff
type FCS = Unit

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
renderFormless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
renderFormless state =
  HH.div_
    [ Format.subHeading_
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
        [ HH.code_
          [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
        , HH.br_
        , if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: case field.result of
                Just (Left str) -> Just str
                _ -> Nothing
            , inputId: label
            }
            [ Input.input
              [ HP.placeholder "Dale"
              , HP.id_ label
              , HP.value field.input
              , HE.onBlur $ HE.input_ $ Formless.HandleBlur (Formless.handleBlur sym)
              , HE.onValueInput $ HE.input \str -> Formless.HandleChange (Formless.handleChange sym str)
              ]
            ]
        ]

renderEmail :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderEmail state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = SProxy :: SProxy "email"
      label = "Email"
   in
      HH.div_
        [ HH.code_
          [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ field.input ]
        , HH.br_
        , if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: case field.result of
                Just (Left str) -> Just str
                _ -> Nothing
            , inputId: label
            }
            [ HH.slot
                unit
                TA.component
                ( TA.Input.defSingle
                  [ HP.placeholder "Search email addresses..." ]
                  [ "not@anemail.org"
                  , "snail@utopia.snailutopia"
                  , "blue@jordans@blordans.pordens"
                  , "yea_that_won't_work@email.com"
                  , "standard@email.com"
                  ]
                  TA.Input.renderItemString
                )
                ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead) )
            ]
        ]
