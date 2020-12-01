module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Basic (basic)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy home
  , Tuple "basic" $ proxy $ H.mkComponent { initialState: identity, eval: H.mkEval H.defaultEval, render: \_ -> HH.slot (SProxy :: SProxy "?") 0 basic {} absurd }
  -- , Tuple "external-components" $ proxy ExternalComponents.component
  -- , Tuple "async" $ proxy Async.component
  -- , Tuple "nested" $ proxy Nested.component
  -- , Tuple "real-world" $ proxy RealWorld.component
  ]
  where
  home =
    H.mkComponent
      { initialState: identity
      , eval: H.mkEval H.defaultEval
      , render
      }
    where
    render _ =
      HH.section_
        [ HH.h1_ [ HH.text "Formless" ]
        , HH.h2_ [ HH.text "A renderless component for painless forms in Halogen" ]
        , HH.section_
            [ HH.p_
                [ HH.text
                    """
                    Formless allows you to write a small, simple spec for your form and receive state updates, validation, dirty states, submission handling, and more for free. You are responsible for providing an initial value and a validation function for every field in your form, but beyond that, Formless will take care of things behind the scenes without ever imposing on how you'd like to render and display your form. You can freely use external Halogen components, add new form behaviors on top (like dependent validation or clearing sets of fields), and more.
                    """
                ]
            , HH.a
                [ HP.href "https://github.com/thomashoneyman/purescript-halogen-formless" ]
                [ HH.text "purescript-halogen-formless" ]
          ]
      ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Formless" } body
