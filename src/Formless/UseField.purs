module Formless.UseField where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Formless.Data.FormFieldResult (FormFieldResult(..), fromEither)
import Formless.Data.FormFieldResult as FormFieldResult
import Halogen.Hooks (Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseDebouncer (UseDebouncer, useDebouncer)

newtype UseField input error a hooks =
  UseField
   (UseDebouncer input
   (UseState Boolean
   (UseState (FormFieldResult error a)
   (UseState Boolean
   (UseState input hooks)))))

derive instance newtypeUseField :: Newtype (UseField i e a hooks) _

type FieldReturn slots output m input error a =
  { input :: input
  , valid :: FormFieldResult error a
  , touched :: Boolean
  , dirty :: Boolean
  , handleInput :: input -> HookM slots output m Unit
  , validate :: HookM slots output m Unit
  , reset :: HookM slots output m Unit
  }

useField
  :: forall slots output m input error a
   . MonadAff m
  => Eq input
  => Eq error
  => Eq a
  => Milliseconds
  -> input
  -> (input -> ExceptT error (HookM slots output m) a)
  -> Hook slots output m (UseField input error a) (FieldReturn slots output m input error a)
useField debounceTime initialInput validator =
  useField' (==) (==) (==) debounceTime initialInput validator

useField'
  :: forall slots output m input error a
   . MonadAff m
  => (input -> input -> Boolean)
  -> (error -> error -> Boolean)
  -> (a -> a -> Boolean)
  -> Milliseconds
  -> input
  -> (input -> ExceptT error (HookM slots output m) a)
  -> Hook slots output m (UseField input error a) (FieldReturn slots output m input error a)
useField' inputEqFn errorEqFn aEqFn debounceTime initialInput validator =
  Hooks.wrap Hooks.do
    input /\ tInput <- useState initialInput
    touched /\ tTouched <- useState false
    valid /\ tValid <- useState NotValidated
    dirty /\ tDirty <- useState false
    setValidate <- useDebouncer debounceTime \finalInput -> do
      Hooks.put tValid Validating
      mbResult <- runExceptT (validator finalInput)
      let newValidation = fromEither mbResult
      Hooks.put tValid newValidation

    Hooks.pure
      { input
      , valid
      , touched
      , dirty
      , handleInput: handleInput tInput tTouched tDirty setValidate
      , validate: validate tInput tValid
      , reset: reset tInput tTouched tDirty tValid
      }
  where
    handleInput tInput tTouched tDirty setValidate newInput = do
      touched' <- Hooks.get tTouched
      unless touched' $ Hooks.put tTouched true

      oldInput <- Hooks.get tInput
      when (not (inputEqFn oldInput newInput)) do
        Hooks.put tInput newInput

        dirty <- Hooks.get tDirty
        let notDirty = not (inputEqFn newInput initialInput)
        when (dirty /= notDirty) do
          Hooks.put tDirty notDirty

        setValidate newInput

    reset tInput tTouched tDirty tValid = do
      Hooks.put tInput initialInput
      Hooks.put tTouched false
      Hooks.put tDirty false
      Hooks.put tValid NotValidated

    validate tInput tValid = do
      input <- Hooks.get tInput
      Hooks.put tValid Validating
      mbResult <- runExceptT (validator input)
      let newValidation = fromEither mbResult
      Hooks.put tValid newValidation

    validationEquals = FormFieldResult.equalsWith errorEqFn aEqFn
