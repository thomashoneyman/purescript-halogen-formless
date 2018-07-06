module App.Form where

import Prelude

import Data.Array (reverse)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Lens as Lens
import Data.Lens.Record as Lens.Record
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | This component will only handle output from Formless to keep
-- | things simple.
data Query a
  = HandleFormless (Formless.Message Query) a

-- | Yea, I know
type State = Unit

-- | Now we can create _this_ component's child query and child slot pairing.
type ChildQuery = Formless.Query Query FCQ FCS Aff
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
          { render: renderFormless }
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
  :: Formless.State
  -> Formless.HTML Query FCQ FCS Aff
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
renderName :: Formless.State -> Formless.HTML Query FCQ FCS Aff
renderName state =
  HH.div_
    ( [ HH.label_
        [ HH.text "Name" ]
      , HH.br_
      , HH.code_
        [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ state.form.inputs.name ]
      , HH.br_
      , HH.input
        [ HP.value state.form.inputs.name
        , HE.onValueInput $ HE.input \str ->
            Formless.HandleChange
              ( ( Lens.set
                  ( Lens.Record.prop (SProxy :: SProxy "form")
                  <<< Lens.Record.prop (SProxy :: SProxy "inputs")
                  <<< Lens.Record.prop (SProxy :: SProxy "name")
                  )
                  str
                )
              <<<
                ( Lens.set
                  ( Lens.Record.prop (SProxy :: SProxy "form")
                  <<< Lens.Record.prop (SProxy :: SProxy "touched")
                  <<< Lens.Record.prop (SProxy :: SProxy "name")
                  )
                  true
                )
              )
        ]
      , HH.br_
      , if state.form.touched.name
          then HH.text "-- changed since form initialization --"
          else HH.text ""
      , HH.br_
      ]
    <>
    case state.form.results.name of
      Nothing -> [ HH.text "" ]
      Just (Left errs) -> map HH.text errs
      Just (Right _) -> [ HH.text "" ]
    <>
    [ HH.br_, HH.br_ ]
    )

renderEmail :: Formless.State -> Formless.HTML Query FCQ FCS Aff
renderEmail state =
  HH.div_
    ( [ HH.label_
        [ HH.text "Email" ]
      , HH.br_
      , HH.code_
        [ HH.text $ fromCharArray <<< reverse <<< toCharArray $ state.form.inputs.email ]
      , HH.br_
      , HH.input
        [ HP.value state.form.inputs.email
        , HE.onValueInput $ HE.input \str ->
            Formless.HandleChange
              ( ( Lens.set
                  ( Lens.Record.prop (SProxy :: SProxy "form")
                  <<< Lens.Record.prop (SProxy :: SProxy "inputs")
                  <<< Lens.Record.prop (SProxy :: SProxy "email")
                  )
                  str
                )
              <<<
                ( Lens.set
                  ( Lens.Record.prop (SProxy :: SProxy "form")
                  <<< Lens.Record.prop (SProxy :: SProxy "touched")
                  <<< Lens.Record.prop (SProxy :: SProxy "email")
                  )
                  true
                )
              )
        ]
      , HH.br_
      , if state.form.touched.email
          then HH.text "-- changed since form initialization --"
          else HH.text ""
      , HH.br_
      ]
    <>
    case state.form.results.email of
      Nothing -> [ HH.text "" ]
      Just (Left errs) -> map HH.text errs
      Just (Right _) -> [ HH.text "" ]
    )
