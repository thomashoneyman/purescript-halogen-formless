module Example.ExternalComponents.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.ExternalComponents.RenderFormless (formless)
import Example.ExternalComponents.Spec (_email, formSpec)
import Example.ExternalComponents.Types (ChildQuery, ChildSlot, Query(..), State)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA
import Ocelot.HTML.Properties (css)

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
  render st =
    HH.div
    [ css "flex-1 container p-12" ]
    [ Format.heading_
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A form leveraging external components." ]
    , HH.slot
        unit
        Formless.component
        { formSpec
        , render: formless
        }
        (const Nothing)
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
        TA.ItemSelected x ->
          (H.query unit $ Formless.handleChange _email x) *> pure a
        _ -> pure a
      TA.VisibilityChanged _ ->
        (H.query unit $ Formless.handleBlur _email) *> pure a
      _ -> pure a
