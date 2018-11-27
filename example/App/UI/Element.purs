module Example.App.UI.Element where

import Prelude

import DOM.HTML.Indexed (HTMLa, HTMLbutton, HTMLinput, HTMLtextarea)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant)
import Example.App.Validation (class ToText, toText)
import Example.App.Validation as V
import Formless (FormFieldResult(..))
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Record.Builder as Builder
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)

type Plain i p = Array (HH.HTML i p) -> HH.HTML i p

css :: ∀ r t. String -> HH.IProp ( "class" :: String | r ) t
css = HP.class_ <<< HH.ClassName

----------
-- Typography

h1_ :: ∀ i p. Plain i p
h1_ = HH.h1 [ css "title" ]

h2_ :: ∀ i p. Plain i p
h2_ = HH.h2 [ css "subtitle is-size-4 has-text-grey" ]

p_ :: ∀ i p. String -> HH.HTML i p
p_ str = HH.p_ [ HH.text str ]

a :: ∀ i p. Array (HH.IProp HTMLa p) -> Plain i p
a props = HH.a ([ css "has-text-blue" ] <> props)

----------
-- Layout

section_ :: ∀ i p. Plain i p
section_ content =
  HH.section
  [ css "section columns" ]
  [ HH.div
    [ css "column is-11-tablet is-three-fifths-desktop" ]
    content
  ]

formContent_ :: ∀ i p. Plain i p
formContent_ content =
  HH.div
  [ css "content" ]
  [ HH.div
    [ css "column has-background-white-bis" ]
    content
  ]

content_ :: ∀ i p. Plain i p
content_ = HH.div [ css "content" ]

----------
-- Buttons

button :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
button props = HH.button ([ css "button is-light" ] <> props)

buttonDark :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonDark props = HH.button ([ css "button is-dark" ] <> props)

buttonPrimary :: ∀ i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonPrimary props = HH.button ([ css "button is-link" ] <> props)

----------
-- Form

grouped_ :: ∀ i p. Plain i p
grouped_ array =
  HH.div
  [ css "field is-grouped" ]
  ( wrap <$> array )

  where
    wrap x = HH.p [ css "control" ] [ x ]

field :: ∀ i p. { label :: String, help :: Either String String } -> Plain i p
field config contents =
  HH.div
  [ css "field" ]
  [ HH.div
    [ css "label" ]
    [ HH.text config.label ]
  , HH.div
    [ css "control" ]
    contents
  , case config.help of
      Left str -> helpError_ str
      Right str -> help_ str
  ]
  where
    help_ str = HH.p [ css "help" ] [ HH.text str ]
    helpError_ str = HH.p [ css "help is-danger" ] [ HH.text str ]


----------
-- Formless

-- Render a result as help text
resultToHelp :: ∀ t e. ToText e => String -> FormFieldResult e t -> Either String String
resultToHelp str = case _ of
  NotValidated -> Right str
  Validating -> Right "validating..."
  other -> maybe (Right str) Left $ V.showError other

-- Provide your own label, error or help text, and placeholder
type FieldConfig' =
  { label :: String
  , help :: Either String String
  , placeholder :: String
  }

-- Provide a label, help text, placeholder, and symbol to have Formless wire everything
-- up on your behalf.
type FieldConfig sym =
  { label :: String
  , help :: String
  , placeholder :: String
  , sym :: SProxy sym
  }

input :: ∀ i p. FieldConfig' -> Array (HH.IProp HTMLinput p) -> HH.HTML i p
input config props =
  field
    { label: config.label, help: config.help }
    [ HH.input
      ( [ HP.type_ InputText
        , either (const $ css "input is-danger") (const $ css "input") config.help
        , HP.placeholder config.placeholder
        ] <> props
      )
    ]

textarea :: ∀ i p. FieldConfig' -> Array (HH.IProp HTMLtextarea p) -> HH.HTML i p
textarea config props =
  field
    { label: config.label, help: config.help }
    [ HH.textarea
      ( [ either (const $ css "textarea is-danger") (const $ css "textarea") config.help
        , HP.placeholder config.placeholder
        ] <> props
      )
    ]

-- Already ready to work with Formless
formlessField
  :: ∀ form sym e o t0 t1 m pq cq cs r fields inputs
   . IsSymbol sym
  => ToText e
  => Newtype (form Record F.FormField) (Record fields)
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Cons sym (F.FormField e String o) t0 fields
  => Cons sym (F.InputFunction e String o) t1 inputs
  => ( FieldConfig'
     -> Array ( HH.IProp
                ( value :: String, onBlur :: FocusEvent, onInput :: Event | r)
                ( F.Query pq cq cs form m Unit )
              )
     -> F.HTML pq cq cs form m
     )
  -> FieldConfig sym
  -> F.State form m
  -> F.HTML pq cq cs form m
formlessField fieldType config state = fieldType (Builder.build config' config) props
  where
    config' =
      Builder.delete (SProxy :: SProxy "sym")
      <<< Builder.modify (SProxy :: SProxy "help") (const help')

    help' = maybe (Right config.help) (Left <<< toText) (F.getError config.sym state.form)

    props =
      [ HP.value (F.getInput config.sym state.form)
      , HE.onValueInput $ HE.input $ F.setValidate config.sym
      ]
