module Formless.UseField where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Formless.Data.FormFieldResult (FormFieldResult(..), fromEither)
import Halogen.Hooks (Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseDebouncer (UseDebouncer, useDebouncer)

newtype UseField input error a hooks =
  UseField (UseDebouncer input (UseState (FormFieldResult error a)
    (UseState Boolean (UseState input hooks))))

derive instance newtypeUseField :: Newtype (UseField i e a hooks) _

type FieldReturn slots output m input error a =
  { input :: input
  , valid :: FormFieldResult error a
  , touched :: Boolean
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
    setValidate <- useDebouncer debounceTime \finalInput -> do
      mbResult <- runExceptT (validator finalInput)
      Hooks.put tValid (fromEither mbResult)

    Hooks.pure
      { input
      , valid
      , touched
      , handleInput: handleInput tInput tTouched setValidate
      , validate: validate tInput tValid
      , reset: reset tInput tTouched tValid
      }
  where
    handleInput tInput tTouched setValidate newInput = do
      touched' <- Hooks.get tTouched
      unless touched' $ Hooks.put tTouched true
      oldInput <- Hooks.get tInput
      when (not (inputEqFn oldInput newInput)) do
        Hooks.put tInput newInput
        setValidate newInput

    reset tInput tTouched tValid = do
      Hooks.put tInput initialInput
      Hooks.put tTouched false
      Hooks.put tValid NotValidated

    validate tInput tValid = do
      input <- Hooks.get tInput
      mbResult <- runExceptT (validator input)
      oldValidation <- Hooks.get tValid
      let newValidation = fromEither mbResult
      when (not (oldValidation `equals` newValidation)) do
        Hooks.put tValid newValidation

    equals l r = case l, r of
      Success l', Success r' -> aEqFn l' r'
      Error l', Error r' -> errorEqFn l' r'
      _, _ -> true
