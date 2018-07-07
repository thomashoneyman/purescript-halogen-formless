module Example.ExternalComponents.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.ExternalComponents.RenderFormless (renderFormless)
import Example.ExternalComponents.Spec (form)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), State)
import Formless as Formless
import Formless.Spec (formSpecToInputFields)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA

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
      [ css "p-12 container mx-auto flex flex-1" ]
      [ HH.div
        [ css "flex-1 mx-10 mt-10" ]
        [ Format.heading_
          [ HH.text "Formless" ]
        , HH.slot
            unit
            Formless.component
              -- TODO: should be able to just pass the spec in...
            { formSpec: formSpecToInputFields form
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
      TA.SelectionsChanged s _ -> case s of
        TA.ItemSelected x -> do
          _ <- H.query unit
                $ Formless.HandleChange
                ( Formless.handleChange (SProxy :: SProxy "email") x ) unit
          pure a
        _ -> pure a
      TA.VisibilityChanged _ -> do
        _ <- H.query unit
              $ Formless.HandleBlur
              ( Formless.handleBlur (SProxy :: SProxy "email") ) unit
        pure a
      _ -> pure a
