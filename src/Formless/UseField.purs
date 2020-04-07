module Formless.UseField where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
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
    (UseState Boolean (UseState (Maybe input) hooks))))

derive instance newtypeUseField :: Newtype (UseField i e a hooks) _

type FieldReturn slots output m input error a =
  { input :: Maybe input
  , validation :: FormFieldResult error a
  , touched :: Boolean
  , handleInput :: input -> HookM slots output m Unit
  , validate :: HookM slots output m Unit
  , reset :: HookM slots output m Unit
  }

useField
  :: forall slots output m input error a
   . MonadAff m
  => Eq error
  => Eq a
  => Milliseconds
  -> Maybe input
  -> (input -> ExceptT error (HookM slots output m) a)
  -> Hook slots output m (UseField input error a) (FieldReturn slots output m input error a)
useField debounceTime initialInput validator =
  useField' (==) (==) debounceTime initialInput validator

useField'
  :: forall slots output m input error a
   . MonadAff m
  => (error -> error -> Boolean)
  -> (a -> a -> Boolean)
  -> Milliseconds
  -> Maybe input
  -> (input -> ExceptT error (HookM slots output m) a)
  -> Hook slots output m (UseField input error a) (FieldReturn slots output m input error a)
useField' errorEqFn aEqFn debounceTime initialInput validator =
  Hooks.wrap Hooks.do
    input /\ tInput <- useState initialInput
    touched /\ tTouched <- useState false
    validation /\ tValidation <- useState NotValidated
    setValidate <- useDebouncer debounceTime \finalInput -> do
      touched' <- Hooks.get tTouched
      unless touched' $ Hooks.put tTouched true
      Hooks.put tInput (Just finalInput)
      mbResult <- runExceptT (validator finalInput)
      Hooks.put tValidation (fromEither mbResult)

    Hooks.pure
      { input
      , validation
      , touched
      , handleInput: handleInput tTouched setValidate
      , validate: validate tInput tValidation
      , reset: reset tInput tTouched tValidation
      }
  where
    handleInput tTouched setValidate input = do
      touched' <- Hooks.get tTouched
      unless touched' $ Hooks.put tTouched true
      setValidate input

    reset tInput tTouched tValidation = do
      Hooks.put tInput initialInput
      Hooks.put tTouched false
      Hooks.put tValidation NotValidated

    validate tInput tValidation = do
      input <- Hooks.get tInput
      for_ input \i -> do
        mbResult <- runExceptT (validator i)
        oldValidation <- Hooks.get tValidation
        let newValidation = fromEither mbResult
        when (not (oldValidation `equals` newValidation)) do
          Hooks.put tValidation newValidation

    equals l r = case l, r of
      Success l', Success r' -> aEqFn l' r'
      Error l', Error r' -> errorEqFn l' r'
      _, _ -> true
