module Example.App.UI.Element where

import Prelude

import DOM.HTML.Indexed (HTMLa, HTMLbutton, HTMLinput, HTMLtextarea)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
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

class_ :: forall r t. String -> HH.IProp ( "class" :: String | r ) t
class_ = HP.class_ <<< HH.ClassName

----------
-- Typography

h1_ :: forall i p. Plain i p
h1_ = HH.h1 [ class_ "title" ]

h2_ :: forall i p. Plain i p
h2_ = HH.h2 [ class_ "subtitle is-size-4 has-text-grey" ]

p_ :: forall i p. String -> HH.HTML i p
p_ str = HH.p_ [ HH.text str ]

a :: forall i p. Array (HH.IProp HTMLa p) -> Plain i p
a props = HH.a ([ class_ "has-text-blue" ] <> props)

----------
-- Layout

section_ :: forall i p. Plain i p
section_ content =
  HH.section
    [ class_ "section columns" ]
    [ HH.div
        [ class_ "column is-11-tablet is-three-fifths-desktop" ]
        content
    ]

formContent_ :: forall i p. Plain i p
formContent_ content =
  HH.div
    [ class_ "content" ]
    [ HH.div
        [ class_ "column has-background-white-bis" ]
        content
    ]

content_ :: forall i p. Plain i p
content_ = HH.div [ class_ "content" ]

----------
-- Buttons

button :: forall i p. Array (HH.IProp HTMLbutton p) -> Plain i p
button props = HH.button ([ class_ "button is-light" ] <> props)

buttonDark :: forall i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonDark props = HH.button ([ class_ "button is-dark" ] <> props)

buttonPrimary :: forall i p. Array (HH.IProp HTMLbutton p) -> Plain i p
buttonPrimary props = HH.button ([ class_ "button is-link" ] <> props)

----------
-- Form

grouped_ :: forall i p. Plain i p
grouped_ array =
  HH.div
    [ class_ "field is-grouped" ]
    ( wrap <$> array )
  where
  wrap x = HH.p [ class_ "control" ] [ x ]

field :: forall i p. { label :: String, help :: Either String String } -> Plain i p
field config contents =
  HH.div
    [ class_ "field" ]
    [ HH.div
        [ class_ "label" ]
        [ HH.text config.label ]
    , HH.div
        [ class_ "control" ]
        contents
    , case config.help of
        Left str -> helpError_ str
        Right str -> help_ str
    ]
  where
    help_ str = HH.p [ class_ "help" ] [ HH.text str ]
    helpError_ str = HH.p [ class_ "help is-danger" ] [ HH.text str ]

----------
-- Formless

-- Render a result as help text
resultToHelp :: forall t e. ToText e => String -> FormFieldResult e t -> Either String String
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

input :: forall i p. FieldConfig' -> Array (HH.IProp HTMLinput p) -> HH.HTML i p
input config props =
  field
    { label: config.label, help: config.help }
    [ HH.input $
        [ HP.type_ InputText
        , either (const $ class_ "input is-danger") (const $ class_ "input") config.help
        , HP.placeholder config.placeholder
        ] <> props
    ]

textarea :: forall i p. FieldConfig' -> Array (HH.IProp HTMLtextarea p) -> HH.HTML i p
textarea config props =
  field
    { label: config.label, help: config.help }
    [ HH.textarea $
        [ config.help # either 
            (const $ class_ "textarea is-danger") 
            (const $ class_ "textarea")
        , HP.placeholder config.placeholder
        ] <> props
    ]

-- Already ready to work with Formless
formlessField
  :: forall form st act ps m sym e o t0 t1 r fields inputs
   . IsSymbol sym
  => ToText e
  => Newtype (form Record F.FormField) { | fields }
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Cons sym (F.FormField e String o) t0 fields
  => Cons sym (F.InputFunction e String o) t1 inputs
  => (FieldConfig'
       -> Array (HH.IProp
                  (value :: String, onBlur :: FocusEvent, onInput :: Event | r)
                  (F.Action form act)
                )
       -> F.ComponentHTML form act ps m
     )
  -> FieldConfig sym
  -> F.PublicState form st
  -> F.ComponentHTML form act ps m
formlessField fieldType config state = fieldType (Builder.build config' config) props
  where
    config' =
      Builder.delete (SProxy :: SProxy "sym")
        >>> Builder.modify (SProxy :: SProxy "help") (const help')

    help' = 
      maybe (Right config.help) (Left <<< toText) (F.getError config.sym state.form)

    props =
      [ HP.value (F.getInput config.sym state.form)
      , HE.onValueInput (Just <<< F.setValidate config.sym)
      ]

